/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.input;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import javax.swing.*;
import javax.swing.undo.UndoManager;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import org.jdesktop.swingx.JXTextArea;

import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.InputUserMessage;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.ResourceHandle;
import uno.anahata.asi.agi.resource.ResourceManager;
import uno.anahata.asi.agi.status.AgiStatus;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.AgiTransferHandler;
import uno.anahata.asi.swing.icons.AttachIcon;
import uno.anahata.asi.swing.icons.CancelIcon;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.FramesIcon;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.icons.RestartIcon;
import uno.anahata.asi.swing.icons.RunAndSendIcon;
import uno.anahata.asi.swing.icons.ScreenshotIcon;
import uno.anahata.asi.swing.icons.SendIcon;
import uno.anahata.asi.swing.internal.AnyChangeDocumentListener;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.swing.internal.UICapture;
import uno.anahata.asi.swing.audio.MicrophonePanel;

/**
 * A fully functional and responsive user input component for the V2 agi.
 * <p>
 * This panel manages a "live" {@link InputUserMessage} object that is updated
 * in real-time as the user types. It leverages the reactive {@link uno.anahata.asi.swing.internal.EdtPropertyChangeListener}
 * pattern, ensuring the preview panel updates automatically without manual rendering calls.
 * </p>
 * <p>
 * <b>Context Intent:</b> Files dropped or attached via the "Attach" button are 
 * automatically registered as managed resources in the {@link ResourceManager}, 
 * while screenshots and application frames are attached directly to the message history.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@Getter
public class InputPanel extends JPanel {

    /** The parent agi panel. */
    private final AgiPanel agiPanel;
    /** The agi session orchestrator. */
    private Agi agi;

    /** The text area for user input. */
    private JXTextArea inputTextArea;
    /** The button to send the message. */
    private JButton sendButton;
    /** The button to decline pending tools and send. */
    private JButton declineAndSendButton;
    /** The button to stop the current API call. */
    private JButton stopButton;
    /** The button to attach files. */
    private JButton attachButton;
    /** The button to attach a desktop screenshot. */
    private JButton screenshotButton;
    /** The button to capture and attach application frames. */
    private JButton captureFramesButton;
    /** The renderer for the live message preview. */
    private InputUserMessagePanel inputMessagePreview;
    /** The scroll pane for the preview renderer. */
    private JScrollPane previewScrollPane;
    /** The split pane separating input and preview. */
    private JSplitPane splitPane; 
    /** The panel for voice input. */
    private MicrophonePanel microphonePanel; 
    
    /** Panel to display the staged message. */
    private JPanel stagedMessagePanel;
    private JLabel stagedMessageLabel;
    private JButton revertStagedButton;
    private JButton deleteStagedButton;
    
    /** Label for transient registration notifications. */
    private JLabel notificationLabel;

    private EdtPropertyChangeListener stagedListener;
    private EdtPropertyChangeListener statusListener;
    
    private final UndoManager undoManager = new UndoManager();

    /**
     * The "live" message being composed by the user. This is the single source
     * of truth for the current input.
     */
    protected InputUserMessage currentMessage;

    /**
     * Constructs a new InputPanel.
     *
     * @param agiPanel The parent agi panel.
     */
    public InputPanel(AgiPanel agiPanel) {
        super(new BorderLayout(5, 5)); 
        this.agiPanel = agiPanel;
        this.agi = agiPanel.getAgi();
        initComponents();
        
        this.stagedListener = new EdtPropertyChangeListener(this, agi, "stagedUserMessage", evt -> updateStagedMessageUI());
        this.statusListener = new EdtPropertyChangeListener(this, agi.getStatusManager(), "currentStatus", evt -> updateSendButtonState());
    }

    /**
     * Initializes the UI components and sets up the real-time model binding.
     */
    private void initComponents() {
        setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5)); 

        inputTextArea = new JXTextArea("Type your message here (Ctrl+Enter to send)");
        inputTextArea.setLineWrap(true);
        inputTextArea.setWrapStyleWord(true);

        // --- UNDO / REDO ---
        inputTextArea.getDocument().addUndoableEditListener(undoManager);
        
        InputMap im = inputTextArea.getInputMap(JComponent.WHEN_FOCUSED);
        ActionMap am = inputTextArea.getActionMap();

        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_Z, KeyEvent.CTRL_DOWN_MASK), "undo");
        am.put("undo", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (undoManager.canUndo()) {
                    undoManager.undo();
                }
            }
        });

        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_Y, KeyEvent.CTRL_DOWN_MASK), "redo");
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_Z, KeyEvent.CTRL_DOWN_MASK | KeyEvent.SHIFT_DOWN_MASK), "redo");
        am.put("redo", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (undoManager.canRedo()) {
                    undoManager.redo();
                }
            }
        });

        // --- FILE DROP SUPPORT ---
        AgiTransferHandler th = new AgiTransferHandler(agiPanel, inputTextArea.getTransferHandler());
        setTransferHandler(th);
        inputTextArea.setTransferHandler(th);

        // --- REAL-TIME MODEL UPDATE ---
        inputTextArea.getDocument().addDocumentListener(new AnyChangeDocumentListener(this::updateMessageText));

        // Ctrl+Enter to send
        KeyStroke ctrlEnter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, KeyEvent.CTRL_DOWN_MASK);
        im.put(ctrlEnter, "sendMessage");

        am.put("sendMessage", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (sendButton.isEnabled()) {
                    sendMessage();
                } else if (stopButton.isVisible() && stopButton.isEnabled()) {
                    agi.stop();
                }
            }
        });

        JScrollPane inputScrollPane = new JScrollPane(inputTextArea);
        inputScrollPane.setPreferredSize(new Dimension(0, 80));

        // --- PREVIEW PANEL INTEGRATION ---
        this.currentMessage = new InputUserMessage(agi);
        this.inputMessagePreview = new InputUserMessagePanel(agiPanel, currentMessage);

        previewScrollPane = new JScrollPane(inputMessagePreview);
        previewScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        previewScrollPane.setPreferredSize(new Dimension(0, 150)); 
        previewScrollPane.setMinimumSize(new Dimension(0, 100)); 

        // --- HORIZONTAL SPLIT PANE ---
        splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, inputScrollPane, previewScrollPane);
        splitPane.setResizeWeight(0.5); 
        splitPane.setDividerLocation(0.5); 
        splitPane.setOneTouchExpandable(true);

        // --- STAGED MESSAGE PANEL ---
        stagedMessagePanel = new JPanel(new BorderLayout(5, 0));
        stagedMessagePanel.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createMatteBorder(1, 0, 1, 0, Color.LIGHT_GRAY),
                BorderFactory.createEmptyBorder(2, 5, 2, 5)
        ));
        stagedMessagePanel.setBackground(new Color(220, 235, 255));
        stagedMessagePanel.setVisible(false);

        stagedMessageLabel = new JLabel("Staged Message: ");
        stagedMessageLabel.setFont(stagedMessageLabel.getFont().deriveFont(Font.ITALIC));
        
        JPanel stagedButtons = new JPanel(new FlowLayout(FlowLayout.RIGHT, 5, 0));
        stagedButtons.setOpaque(false);
        
        revertStagedButton = new JButton("Edit", new RestartIcon(16));
        revertStagedButton.setToolTipText("Move staged message back to input for editing");
        revertStagedButton.addActionListener(e -> revertStagedMessage());
        
        deleteStagedButton = new JButton(new DeleteIcon(16));
        deleteStagedButton.setToolTipText("Delete staged message");
        deleteStagedButton.addActionListener(e -> deleteStagedMessage());
        
        stagedButtons.add(revertStagedButton);
        stagedButtons.add(deleteStagedButton);
        
        stagedMessagePanel.add(stagedMessageLabel, BorderLayout.CENTER);
        stagedMessagePanel.add(stagedButtons, BorderLayout.EAST);

        add(splitPane, BorderLayout.CENTER);

        JPanel southContainer = new JPanel(new BorderLayout(0, 5));
        southContainer.setOpaque(false);
        
        southContainer.add(stagedMessagePanel, BorderLayout.NORTH);

        JPanel southButtonPanel = new JPanel(new BorderLayout(5, 0));
        southButtonPanel.setOpaque(false);

        JPanel actionButtonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
        actionButtonPanel.setOpaque(false);

        microphonePanel = new MicrophonePanel(this);
        actionButtonPanel.add(microphonePanel);

        attachButton = new JButton(new AttachIcon(16));
        attachButton.setToolTipText("Add Managed Resources (V2)");
        attachButton.addActionListener(e -> attachFiles());

        screenshotButton = new JButton(new ScreenshotIcon(16));
        screenshotButton.setToolTipText("Attach Desktop Screenshot (History)");
        screenshotButton.addActionListener(e -> attachScreenshot());

        captureFramesButton = new JButton(new FramesIcon(16));
        captureFramesButton.setToolTipText("Attach Application Frames (History)");
        captureFramesButton.addActionListener(e -> attachWindowCaptures());

        actionButtonPanel.add(attachButton);
        actionButtonPanel.add(screenshotButton);
        actionButtonPanel.add(captureFramesButton);

        notificationLabel = new JLabel("");
        notificationLabel.setForeground(new Color(0, 120, 0));
        notificationLabel.setFont(notificationLabel.getFont().deriveFont(Font.BOLD));
        notificationLabel.setVisible(false);
        actionButtonPanel.add(notificationLabel);

        JPanel eastButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 5, 0));
        eastButtonPanel.setOpaque(false);
        
        declineAndSendButton = new JButton("Decline Pending & Send", new CancelIcon(16));
        declineAndSendButton.addActionListener(e -> declinePendingAndSend());
        declineAndSendButton.setVisible(false);
        
        sendButton = new JButton("Send", new SendIcon(16));
        sendButton.addActionListener(e -> sendMessage());
        
        stopButton = new JButton("Stop", IconUtils.getIcon("delete.png", 16, 16));
        stopButton.addActionListener(e -> agi.stop());
        stopButton.setVisible(false);

        eastButtonPanel.add(declineAndSendButton);
        eastButtonPanel.add(sendButton);
        eastButtonPanel.add(stopButton);

        southButtonPanel.add(actionButtonPanel, BorderLayout.WEST);
        southButtonPanel.add(eastButtonPanel, BorderLayout.EAST);

        southContainer.add(southButtonPanel, BorderLayout.CENTER);
        add(southContainer, BorderLayout.SOUTH);
        
        updateStagedMessageUI();
        updateSendButtonState();
    }

    /**
     * Resynchronizes the input panel with the current {@link Agi} session state.
     * <p>
     * This method is invoked to ensure the UI component is observing the correct 
     * {@link Agi} instance. It re-initializes reactive property change listeners,
     * clears current input buffers, and updates the send button and staged message 
     * visibility to match the new session's state.
     * </p>
     */
    public void reload() {
        this.agi = agiPanel.getAgi();
        
        if (stagedListener != null) {
            stagedListener.unbind();
        }
        this.stagedListener = new EdtPropertyChangeListener(this, agi, "stagedUserMessage", evt -> updateStagedMessageUI());
        
        if (statusListener != null) {
            statusListener.unbind();
        }
        this.statusListener = new EdtPropertyChangeListener(this, agi.getStatusManager(), "currentStatus", evt -> updateSendButtonState());

        resetMessage();
        updateStagedMessageUI();
        updateSendButtonState();
    }

    /** Updates the underlying message model with the text area content. */
    private void updateMessageText() {
        currentMessage.setText(inputTextArea.getText());
        updateSendButtonState();
    }

    /**
     * Registers a list of file paths as managed V2 resources and provides feedback.
     * 
     * @param paths The paths to register.
     * @param registeredBy A string describing the origin of the registration.
     */
    public void registerPathsAsResources(List<Path> paths, String registeredBy) {
        executeTask("Register Resources", () -> {
            ResourceManager manager = agi.getResourceManager();
            for (Path p : paths) {
                ResourceHandle handle = agi.getConfig().createResourceHandle(p.toUri());
                Resource resource = new Resource(handle);
                manager.register(resource, registeredBy);
            }
            return paths.size();
        }, (count) -> {
            showNotification(count + " resource(s) registered");
        }, (error) -> {
            log.error("Failed to register resources", error);
        });
    }

    /**
     * Displays a transient notification message in the action panel.
     * 
     * @param text The message text.
     */
    private void showNotification(String text) {
        notificationLabel.setText(text);
        notificationLabel.setVisible(true);
        Timer timer = new Timer(3000, e -> {
            notificationLabel.setVisible(false);
            notificationLabel.setText("");
        });
        timer.setRepeats(false);
        timer.start();
    }

    /**
     * Attaches a single path to the current history message (Used by Mic).
     * @param p The path.
     * @throws Exception if attachment fails.
     */
    public void attach(Path p) throws Exception {
        currentMessage.addAttachment(p);
        updateSendButtonState();
        scrollToBottomPreview();
    }

    /** Opens file chooser and registers selected files as resources. */
    private void attachFiles() {
        JFileChooser fileChooser = new JFileChooser();
        fileChooser.setMultiSelectionEnabled(true);
        if (fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
            File[] selectedFiles = fileChooser.getSelectedFiles();
            registerPathsAsResources(Arrays.stream(selectedFiles).map(File::toPath).toList(), "added to context by user via attach button");
        }
    }

    /** Captures desktop screenshot and attaches to history. */
    private void attachScreenshot() {
        executeTask("Attach Screenshot", () -> {
            List<Path> files = UICapture.screenshotAllScreens();
            currentMessage.addAttachments(files);
            scrollToBottomPreview();
            return null;
        });
    }

    /** Captures application frames and attaches to history. */
    private void attachWindowCaptures() {
        executeTask("Attach Application Frames", () -> {
            List<Path> files = UICapture.screenshotAllWindows();
            currentMessage.addAttachments(files);
            scrollToBottomPreview();
            return null;
        });
    }

    /** Sends the current message asynchronously. */
    private void sendMessage() {
        setButtonsEnabled(false);
        final InputUserMessage messageToSend = this.currentMessage; 
        resetMessage();
        executeTask("Send Message", () -> {
            agi.sendMessage(messageToSend);
            return null;
        }, (result) -> {
            setButtonsEnabled(true);
            SwingUtilities.invokeLater(() -> inputTextArea.requestFocusInWindow());
        }, (error) -> {
            setButtonsEnabled(true);
        });
    }

    private void declinePendingAndSend() {
        AbstractModelMessage promptMsg = agi.getToolPromptMessage();
        if (promptMsg != null) {
            promptMsg.declineAllPending();
        }
        sendMessage();
    }

    /**
     * Synchronizes the staged message panel visibility and content with the current {@link Agi} state.
     * <p>
     * If a message is staged (e.g., waiting for user review after a tool call), this method
     * updates the summary label with a preview of the text and makes the panel visible.
     * Otherwise, it hides the panel.
     * </p>
     */
    private void updateStagedMessageUI() {
        InputUserMessage staged = agi.getStagedUserMessage();
        if (staged != null) {
            String text = staged.getText();
            if (text.length() > 50) {
                text = text.substring(0, 47) + "...";
            }
            stagedMessageLabel.setText("Staged Message: " + text);
            stagedMessagePanel.setVisible(true);
        } else {
            stagedMessagePanel.setVisible(false);
        }
        revalidate();
        repaint();
    }

    /**
     * Updates the state and text of the send and stop buttons based on the current {@link AgiStatus}.
     * <p>
     * This method handles the logic for switching between the "Send" and "Run Pending & Send"
     * states, manages the visibility of the "Stop" button during active API calls, and
     * ensures the "Decline Pending & Send" button is correctly toggled when tools are pending.
     * </p>
     */
    private void updateSendButtonState() {
        AgiStatus status = agi.getStatusManager().getCurrentStatus();
        boolean isApiActive = status == AgiStatus.AWAKENING_KUNDALINI || status == AgiStatus.API_CALL_IN_PROGRESS || status == AgiStatus.WAITING_WITH_BACKOFF;
        stopButton.setVisible(isApiActive);
        stopButton.setEnabled(isApiActive);
        boolean canSend = status != AgiStatus.CANDIDATE_CHOICE_PROMPT;
        sendButton.setEnabled(canSend);
        if (status == AgiStatus.TOOL_PROMPT) {
            sendButton.setText("Run Pending & Send");
            sendButton.setIcon(new RunAndSendIcon(16));
            declineAndSendButton.setVisible(true);
        } else {
            sendButton.setText("Send");
            sendButton.setIcon(new SendIcon(16));
            declineAndSendButton.setVisible(false);
        }
    }

    private void revertStagedMessage() {
        InputUserMessage staged = agi.getStagedUserMessage();
        if (staged != null) {
            agi.setStagedUserMessage(null);
            this.currentMessage = staged;
            inputTextArea.setText(staged.getText());
            InputUserMessagePanel newRenderer = new InputUserMessagePanel(agiPanel, this.currentMessage);
            previewScrollPane.setViewportView(newRenderer);
            this.inputMessagePreview = newRenderer;
            updateSendButtonState();
            SwingUtilities.invokeLater(() -> inputTextArea.requestFocusInWindow());
        }
    }

    private void deleteStagedMessage() {
        agi.setStagedUserMessage(null);
    }

    /** Trigger high-fidelity scroll to the bottom of the preview area. */
    public void scrollToBottomPreview() {
        inputMessagePreview.scrollToBottom();
    }

    private <T> void executeTask(String taskName, Callable<T> backgroundTask) {
        new SwingTask<>(this, taskName, backgroundTask).execute();
    }

    private <T> void executeTask(String taskName, Callable<T> backgroundTask, Consumer<T> onDone, Consumer<Exception> onError) {
        new SwingTask<>(this, taskName, backgroundTask, onDone, onError).execute();
    }

    private void resetMessage() {
        this.currentMessage = new InputUserMessage(agi);
        inputTextArea.setText("");
        undoManager.discardAllEdits();
        InputUserMessagePanel newRenderer = new InputUserMessagePanel(agiPanel, this.currentMessage);
        previewScrollPane.setViewportView(newRenderer);
        this.inputMessagePreview = newRenderer;
        updateSendButtonState();
    }

    private void setButtonsEnabled(boolean enabled) {
        attachButton.setEnabled(enabled);
        screenshotButton.setEnabled(enabled);
        captureFramesButton.setEnabled(enabled);
        microphonePanel.setMicrophoneComponentsEnabled(enabled);
        declineAndSendButton.setEnabled(enabled);
    }
}
