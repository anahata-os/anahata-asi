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
import javax.swing.*;
import javax.swing.undo.UndoManager;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import org.jdesktop.swingx.JXTextArea;

import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.InputUserMessage;
import uno.anahata.asi.agi.message.UserMessage;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.ResourceHandle;
import uno.anahata.asi.agi.resource.ResourceManager;
import uno.anahata.asi.agi.status.AgiStatus;
import uno.anahata.asi.swing.agi.SwingAgiConfig;
import uno.anahata.asi.swing.agi.AgiTransferHandler;
import uno.anahata.asi.swing.icons.ActionIconKey;
import uno.anahata.asi.swing.internal.AnyChangeDocumentListener;
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.swing.internal.UICapture;
import uno.anahata.asi.swing.audio.MicrophonePanel;
import java.net.URI;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.components.ExceptionDialog;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;

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
    /**
     * Button to add a URL-based context resource to the active session.
     */
    private JButton addUrlButton;
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
    /**
     * Label displaying summary preview of the staged user message.
     */
    private JLabel stagedMessageLabel;
    /**
     * Button to edit and unstage the staged user message.
     */
    private JButton revertStagedButton;
    /**
     * Button to permanently discard the staged user message.
     */
    private JButton deleteStagedButton;
    
    /** Label for transient registration notifications. */
    private JLabel notificationLabel;

    /**
     * Reactive property change listener for staged user messages.
     */
    private EdtPropertyChangeListener stagedListener;
    /**
     * Reactive property change listener for session status changes.
     */
    private EdtPropertyChangeListener statusListener;

    /**
     * Reactive property change listener for active turn message updates.
     */
    private EdtPropertyChangeListener activeTurnListener;
    /**
     * Reactive property change listener for remaining tools countdown updates.
     */
    private EdtPropertyChangeListener remainingToolsListener;
    /**
     * UndoManager tracking edits within the input text area.
     */
    private final UndoManager undoManager = new UndoManager();

    /**
     * The "live" message being composed by the user. This is the single source
     * of truth for the current input.
     */
    protected InputUserMessage currentMessage;

    /**
     * Constructs a new InputPanel with reactive listeners for status, staged
     * messages, and live tool countdowns.
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
        this.activeTurnListener = new EdtPropertyChangeListener(this, agi, "activeTurnMessage", evt -> {
            bindRemainingToolsListener(agi.getToolPromptMessage());
            updateSendButtonState();
        });
        bindRemainingToolsListener(agi.getToolPromptMessage());
    }

    /**
     * Initializes the UI components and sets up layout with staged message
     * panel in NORTH, split pane in CENTER, and action buttons in SOUTH.
     */
    private void initComponents() {
        setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        inputTextArea = new JXTextArea("Type, paste or drop here (images supported).... Press Ctrl+Enter to send");
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
                    handleSendAction();
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

        SwingAgiConfig config = agiPanel.getAgiConfig();

        // --- STAGED MESSAGE PANEL (Placed in NORTH above split pane) ---
        stagedMessagePanel = new JPanel(new BorderLayout(5, 0));
        stagedMessagePanel.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createMatteBorder(0, 0, 1, 0, config.getTheme().getChromeBorder()),
                BorderFactory.createEmptyBorder(2, 5, 2, 5)
        ));
        stagedMessagePanel.setBackground(config.getTheme().getChipBackground());
        stagedMessagePanel.setVisible(false);

        stagedMessageLabel = new JLabel("Staged Message: ");
        stagedMessageLabel.setFont(stagedMessageLabel.getFont().deriveFont(Font.ITALIC));

        JPanel stagedButtons = new JPanel(new FlowLayout(FlowLayout.RIGHT, 5, 0));
        stagedButtons.setOpaque(false);

        revertStagedButton = new JButton("Edit", config.getActionIcon(ActionIconKey.EDIT, 16));
        revertStagedButton.setToolTipText("Move staged message back to input for editing");
        revertStagedButton.addActionListener(e -> editStagedMessage());

        deleteStagedButton = config.createSquareButton(ActionIconKey.DELETE, 16, "Delete staged message");
        deleteStagedButton.addActionListener(e -> deleteStagedMessage());

        stagedButtons.add(revertStagedButton);
        stagedButtons.add(deleteStagedButton);

        stagedMessagePanel.add(stagedMessageLabel, BorderLayout.CENTER);
        stagedMessagePanel.add(stagedButtons, BorderLayout.EAST);

        // Layout: Staged Message in NORTH, SplitPane in CENTER, Action Bar in SOUTH
        add(stagedMessagePanel, BorderLayout.NORTH);
        add(splitPane, BorderLayout.CENTER);

        JPanel southButtonPanel = new JPanel(new BorderLayout(5, 0));
        southButtonPanel.setOpaque(false);

        JPanel actionButtonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
        actionButtonPanel.setOpaque(false);

        microphonePanel = new MicrophonePanel(this);
        actionButtonPanel.add(microphonePanel);

        attachButton = config.createSquareButton(ActionIconKey.ATTACH, 16, "Add local file as registered Resource");
        attachButton.addActionListener(e -> registerFilesAsResources());

        addUrlButton = config.createSquareButton(ActionIconKey.LINK, 16, "Add URL Resource as registered Resource");
        addUrlButton.addActionListener(e -> addUrl());

        screenshotButton = config.createSquareButton(ActionIconKey.SCREENSHOT, 16, "Attach Desktop Screenshot to your message");
        screenshotButton.addActionListener(e -> attachScreenshot());

        captureFramesButton = config.createSquareButton(ActionIconKey.CAPTURE_WINDOW, 16, "Attach Screenshot of this application to your message");
        captureFramesButton.addActionListener(e -> attachWindowCaptures());

        actionButtonPanel.add(attachButton);
        actionButtonPanel.add(addUrlButton);
        actionButtonPanel.add(screenshotButton);
        actionButtonPanel.add(captureFramesButton);

        notificationLabel = new JLabel("");
        notificationLabel.setForeground(new Color(0, 120, 0));
        notificationLabel.setFont(notificationLabel.getFont().deriveFont(Font.BOLD));
        notificationLabel.setVisible(false);
        actionButtonPanel.add(notificationLabel);

        JPanel eastButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 5, 0));
        eastButtonPanel.setOpaque(false);

        declineAndSendButton = new JButton("Decline Pending & Send", config.getActionIcon(ActionIconKey.CANCEL, 16));
        declineAndSendButton.addActionListener(e -> declinePendingAndSend());
        declineAndSendButton.setVisible(false);

        sendButton = new JButton("Send", config.getActionIcon(ActionIconKey.SEND, 16));
        sendButton.addActionListener(e -> handleSendAction());

        stopButton = new JButton("Stop", config.getActionIcon(ActionIconKey.STOP, 16));
        stopButton.addActionListener(e -> agi.stop());
        stopButton.setVisible(false);

        eastButtonPanel.add(declineAndSendButton);
        eastButtonPanel.add(sendButton);
        eastButtonPanel.add(stopButton);

        southButtonPanel.add(actionButtonPanel, BorderLayout.WEST);
        southButtonPanel.add(eastButtonPanel, BorderLayout.EAST);

        add(southButtonPanel, BorderLayout.SOUTH);

        updateStagedMessageUI();
        updateSendButtonState();
    }

    /**
     * Resynchronizes the input panel with the current Agi session state,
     * rebinding all listeners including tool countdowns.
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

        if (activeTurnListener != null) {
            activeTurnListener.unbind();
        }
        this.activeTurnListener = new EdtPropertyChangeListener(this, agi, "activeTurnMessage", evt -> {
            bindRemainingToolsListener(agi.getToolPromptMessage());
            updateSendButtonState();
        });
        bindRemainingToolsListener(agi.getToolPromptMessage());

        resetMessage();
        updateStagedMessageUI();
        updateSendButtonState();
    }

    /**
     * Binds the remaining tools property change listener to the active tool
     * prompt message.
     *
     * @param promptMessage The current tool prompt message, or null to unbind.
     */
    private void bindRemainingToolsListener(AbstractModelMessage promptMessage) {
        if (remainingToolsListener != null) {
            remainingToolsListener.unbind();
            remainingToolsListener = null;
        }
        if (promptMessage != null) {
            this.remainingToolsListener = new EdtPropertyChangeListener(this, promptMessage, "remainingTools", evt -> updateSendButtonState());
        }
    }
    /** Updates the underlying message model with the text area content. */
    /** 
     * Updates the underlying message model with the text area content. 
     * This method is triggered by document changes to keep the live {@link InputUserMessage}
     * in sync with the user's keystrokes.
     */
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
    /**
     * Displays a transient notification message in the action panel.
     * These notifications provide non-intrusive feedback for background operations
     * like resource registration.
     * 
     * @param text The message text to display.
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

    /**
     * Triggers a modal dialog prompting for a URL to register as a resource.
     */
    private void addUrl() {
        String url = JOptionPane.showInputDialog(this, "Enter the URL to add as a resource:", "Add URL Resource", JOptionPane.PLAIN_MESSAGE);
        if (url != null && !url.trim().isEmpty()) {
            executeTask("Register URL Resource", () -> {
                try {
                    URI uri = new URI(url.trim());
                    ResourceHandle handle = agi.getConfig().createResourceHandle(uri);
                    Resource resource = new Resource(handle);
                    agi.getResourceManager().register(resource, "added to context by user via add url button");
                    return true;
                } catch (Exception ex) {
                    log.error("Failed to register URL resource: " + url, ex);
                    throw ex;
                }
            }, success -> {
                showNotification("URL resource registered");
            }, error -> {
                ExceptionDialog.show(this, "Add URL Resource", "Failed to register the provided URL as a managed resource.", error);
            });
        }
    }

    /** Opens file chooser and registers selected files as resources. */
    /** 
     * Opens a file chooser dialog and registers selected files as managed resources.
     * This is the primary way for users to add persistent file context to the session.
     */
    private void registerFilesAsResources() {
        JFileChooser fileChooser = new JFileChooser();
        fileChooser.setMultiSelectionEnabled(true);
        if (fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
            File[] selectedFiles = fileChooser.getSelectedFiles();
            registerPathsAsResources(Arrays.stream(selectedFiles).map(File::toPath).toList(), "added to context by user via attach button");
        }
    }

    /** Captures desktop screenshot and attaches to history. */
    /** 
     * Captures a desktop screenshot of all screens and attaches them directly 
     * to the composition history of the current message.
     */
    private void attachScreenshot() {
        executeTask("Attach Screenshot", () -> {
            List<Path> files = UICapture.screenshotAllScreens();
            currentMessage.addAttachments(files);
            scrollToBottomPreview();
            return null;
        });
    }

    /** Captures application frames and attaches to history. */
    /** 
     * Captures high-fidelity frames of all open application windows and attaches 
     * them to the composition history.
     */
    private void attachWindowCaptures() {
        executeTask("Attach Application Frames", () -> {
            List<Path> files = UICapture.screenshotAllWindows();
            currentMessage.addAttachments(files);
            scrollToBottomPreview();
            return null;
        });
    }

    /**
     * Handles the send action, executing pending tools first if in TOOL_PROMPT
     * status, or sending the message directly.
     */
    private void handleSendAction() {
        if (agi.getStatusManager().getCurrentStatus() == AgiStatus.TOOL_PROMPT) {
            AbstractModelMessage promptMsg = agi.getToolPromptMessage();
            if (promptMsg != null && promptMsg.hasPendingTools()) {
                executeTask("Executing Pending Tools", () -> {
                    return promptMsg.executeAllPending();
                }, done -> {
                    sendMessage();
                }, error -> {
                    log.error("Failed to execute pending tools", error);
                });
                return;
            }
        }
        sendMessage();
    }
    /**
     * Orchestrates the asynchronous sending of the current message without disabling input actions, preserving full compose and attach capabilities while background operations or turns are active.
     */
    private void sendMessage() {
        final InputUserMessage messageToSend = this.currentMessage;
        resetMessage();
        executeTask("Send Message", () -> {
            agi.sendMessage(messageToSend);
            return null;
        }, (result) -> {
            SwingUtilities.invokeLater(() -> inputTextArea.requestFocusInWindow());
        }, (error) -> {
        });
    }

    /**
     * Declines all pending tool calls from the previous turn and sends the
     * current message. This provides a "clean slate" shortcut for the user to
     * continue the conversation
     * without approving or manually declining individual tools.
     */
    private void declinePendingAndSend() {
        AbstractModelMessage promptMsg = agi.getToolPromptMessage();
        if (promptMsg != null) {
            promptMsg.declineAllPending();
        }
        sendMessage();
    }

    /**
     * Synchronizes the staged message panel visibility and text preview
     * by delegating directly to UserMessage#getBriefSummary(), ensuring whitespace
     * and attachment indicators are formatted cleanly without raw control
     * characters.
     */
    private void updateStagedMessageUI() {
        UserMessage staged = agi.getStagedUserMessage();
        if (staged != null && !staged.isEmpty()) {
            stagedMessageLabel.setText("Staged Message: " + staged.getBriefSummary());
            stagedMessagePanel.setVisible(true);
        } else {
            stagedMessagePanel.setVisible(false);
        }
        revalidate();
        repaint();
    }

    /**
     * Updates the state, text, and countdown label of the send and stop buttons
     * based on current AgiStatus and remaining tools.
     */
    private void updateSendButtonState() {
        AgiStatus status = agi.getStatusManager().getCurrentStatus();
        boolean isStoppable = status == AgiStatus.AWAKENING_KUNDALINI
                || status == AgiStatus.API_CALL_IN_PROGRESS
                || status == AgiStatus.WAITING_WITH_BACKOFF
                || status == AgiStatus.AUTO_EXECUTING_TOOLS;
        stopButton.setVisible(isStoppable);
        stopButton.setEnabled(isStoppable);

        AbstractModelMessage promptMsg = agi.getToolPromptMessage();
        int remaining = (promptMsg != null) ? promptMsg.getRemainingToolCallsCount() : 0;
        if (status == AgiStatus.AUTO_EXECUTING_TOOLS && remaining > 0) {
            stopButton.setText("Stop Remaining (" + remaining + ")");
        } else {
            stopButton.setText("Stop");
        }

        boolean canSend = status != AgiStatus.CANDIDATE_CHOICE_PROMPT;
        sendButton.setEnabled(canSend);
        if (status == AgiStatus.TOOL_PROMPT && promptMsg != null) {
            sendButton.setText("Run Pending & Send");
            sendButton.setIcon(agiPanel.getAgiConfig().getActionIcon(ActionIconKey.RUN_AND_SEND, 16));
            declineAndSendButton.setVisible(true);
        } else {
            sendButton.setText("Send");
            sendButton.setIcon(agiPanel.getAgiConfig().getActionIcon(ActionIconKey.SEND, 16));
            declineAndSendButton.setVisible(false);
        }
    }

    /**
     * Moves the accumulated text and attachments from the staged message back
     * into the active input buffer for editing.
     */
    private void editStagedMessage() {
        UserMessage staged = agi.getStagedUserMessage();
        if (staged != null) {
            agi.setStagedUserMessage(null);
            if (staged instanceof InputUserMessage stagedInputUserMessage) {
                this.currentMessage = stagedInputUserMessage;
            } else {
                this.currentMessage = new InputUserMessage(agi);
                this.currentMessage.append(staged);
            }
            inputTextArea.setText(currentMessage.getText());
            InputUserMessagePanel newRenderer = new InputUserMessagePanel(agiPanel, this.currentMessage);
            previewScrollPane.setViewportView(newRenderer);
            this.inputMessagePreview = newRenderer;
            updateSendButtonState();
            SwingUtilities.invokeLater(() -> inputTextArea.requestFocusInWindow());
        }
    }

    /**
     * Permanently deletes the current staged message, clearing it from the session state.
     */
    private void deleteStagedMessage() {
        agi.setStagedUserMessage(null);
    }

    /** Trigger high-fidelity scroll to the bottom of the preview area. */
    public void scrollToBottomPreview() {
        inputMessagePreview.scrollToBottom();
    }

    /**
     * Helper to execute a background task with automatic progress tracking in the UI.
     * 
     * @param <T> The result type of the task.
     * @param taskName The descriptive name of the task for logging and UI feedback.
     * @param backgroundTask The actual logic to execute on a background thread.
     */
    private <T> void executeTask(String taskName, Callable<T> backgroundTask) {
        new SwingTask<>(agiPanel, taskName, backgroundTask).start();
    }

    /**
     * Runs a background task with custom completion and error callbacks on the EDT.
     * @param backgroundTask The background logic to execute.
     * @param onError Callback executed on the EDT when the task fails.
     * @param onDone Callback executed on the EDT when the task succeeds.
     * @param taskName The descriptive task name.
     * @param <T> The result type of the task.
     */
    private <T> void executeTask(String taskName, Callable<T> backgroundTask, Consumer<T> onDone, Consumer<Exception> onError) {
        new SwingTask<>(agiPanel, taskName, backgroundTask, onDone, onError, true).start();
    }

    /**
     * Resets the composition state, clearing the text area, undo history, and 
     * creating a fresh {@link InputUserMessage} instance.
     */
    private void resetMessage() {
        this.currentMessage = new InputUserMessage(agi);
        inputTextArea.setText("");
        undoManager.discardAllEdits();
        InputUserMessagePanel newRenderer = new InputUserMessagePanel(agiPanel, this.currentMessage);
        previewScrollPane.setViewportView(newRenderer);
        this.inputMessagePreview = newRenderer;
        updateSendButtonState();
    }

}