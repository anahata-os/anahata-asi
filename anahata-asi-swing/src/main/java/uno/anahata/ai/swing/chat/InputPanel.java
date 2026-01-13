/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
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
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import org.jdesktop.swingx.JXTextArea;

import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.AbstractModelMessage;
import uno.anahata.ai.model.core.AbstractToolMessage;
import uno.anahata.ai.model.core.InputUserMessage;
import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.model.tool.AbstractToolResponse;
import uno.anahata.ai.model.tool.ToolExecutionStatus;
import uno.anahata.ai.status.ChatStatus;
import uno.anahata.ai.swing.components.ScrollablePanel;
import uno.anahata.ai.swing.icons.AutoReplyIcon;
import uno.anahata.ai.swing.icons.DeleteIcon;
import uno.anahata.ai.swing.icons.IconUtils;
import uno.anahata.ai.swing.icons.RestartIcon;
import uno.anahata.ai.swing.icons.RunAndSendIcon;
import uno.anahata.ai.swing.internal.AnyChangeDocumentListener;
import uno.anahata.ai.swing.internal.EdtPropertyChangeListener;
import uno.anahata.ai.swing.internal.SwingTask;
import uno.anahata.ai.swing.internal.UICapture;
import uno.anahata.ai.swing.media.util.MicrophonePanel;

/**
 * A fully functional and responsive user input component for the V2 chat.
 * <p>
 * This panel manages a "live" {@link InputUserMessage} object that is updated
 * in real-time as the user types. It leverages the reactive {@link uno.anahata.ai.swing.internal.EdtPropertyChangeListener}
 * pattern, ensuring the preview panel updates automatically without manual rendering calls.
 *
 * @author anahata
 */
@Slf4j
@Getter
public class InputPanel extends JPanel {

    /** The parent chat panel. */
    private final ChatPanel chatPanel;
    /** The chat session orchestrator. */
    private Chat chat;

    /** The text area for user input. */
    private JXTextArea inputTextArea;
    /** The button to send the message. */
    private JButton sendButton;
    /** The button to stop the current API call. */
    private JButton stopButton;
    /** The button to attach files. */
    private JButton attachButton;
    /** The button to attach a desktop screenshot. */
    private JButton screenshotButton;
    /** The button to capture and attach application frames. */
    private JButton captureFramesButton;
    /** The renderer for the live message preview. */
    private UserInputMessagePanel inputMessagePreview;
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
    
    private EdtPropertyChangeListener stagedListener;
    private EdtPropertyChangeListener statusListener;

    /**
     * The "live" message being composed by the user. This is the single source
     * of truth for the current input.
     */
    private InputUserMessage currentMessage;

    /**
     * Constructs a new InputPanel.
     *
     * @param chatPanel The parent chat panel.
     */
    public InputPanel(ChatPanel chatPanel) {
        super(new BorderLayout(5, 5)); 
        this.chatPanel = chatPanel;
        this.chat = chatPanel.getChat();
        initComponents();
        
        this.stagedListener = new EdtPropertyChangeListener(this, chat, "stagedUserMessage", evt -> updateStagedMessageUI());
        this.statusListener = new EdtPropertyChangeListener(this, chat.getStatusManager(), "currentStatus", evt -> updateSendButtonState());
    }

    /**
     * Initializes the UI components and sets up the real-time model binding.
     */
    private void initComponents() {
        setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5)); 

        inputTextArea = new JXTextArea("Type your message here (Ctrl+Enter to send)");
        inputTextArea.setLineWrap(true);
        inputTextArea.setWrapStyleWord(true);

        // --- REAL-TIME MODEL UPDATE ---
        inputTextArea.getDocument().addDocumentListener(new AnyChangeDocumentListener(this::updateMessageText));

        // Ctrl+Enter to send
        KeyStroke ctrlEnter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, KeyEvent.CTRL_DOWN_MASK);
        inputTextArea.getInputMap(JComponent.WHEN_FOCUSED).put(ctrlEnter, "sendMessage");

        inputTextArea.getActionMap().put("sendMessage", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (sendButton.isEnabled()) {
                    sendMessage();
                } else if (stopButton.isVisible() && stopButton.isEnabled()) {
                    chat.stop();
                }
            }
        });

        JScrollPane inputScrollPane = new JScrollPane(inputTextArea);
        inputScrollPane.setPreferredSize(new Dimension(0, 80));

        // --- PREVIEW PANEL INTEGRATION ---
        this.currentMessage = new InputUserMessage(chat);
        this.inputMessagePreview = new UserInputMessagePanel(chatPanel, currentMessage);

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
        stagedMessagePanel.setBackground(new Color(220, 235, 255)); // Slightly darker blue for visibility
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

        // Panel for buttons and staged message on the south side
        JPanel southContainer = new JPanel(new BorderLayout(0, 5));
        southContainer.setOpaque(false);
        
        southContainer.add(stagedMessagePanel, BorderLayout.NORTH);

        JPanel southButtonPanel = new JPanel(new BorderLayout(5, 0));
        southButtonPanel.setOpaque(false);

        // Panel for action buttons (mic, attach, etc.) on the west
        JPanel actionButtonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
        actionButtonPanel.setOpaque(false);

        microphonePanel = new MicrophonePanel(this);
        actionButtonPanel.add(microphonePanel);

        attachButton = new JButton(IconUtils.getIcon("attach.png"));
        attachButton.setToolTipText("Attach Files");
        attachButton.addActionListener(e -> attachFiles());

        screenshotButton = new JButton(IconUtils.getIcon("desktop_screenshot.png"));
        screenshotButton.setToolTipText("Attach Desktop Screenshot");
        screenshotButton.addActionListener(e -> attachScreenshot());

        captureFramesButton = new JButton(IconUtils.getIcon("capture_frames.png"));
        captureFramesButton.setToolTipText("Attach Application Frames");
        captureFramesButton.addActionListener(e -> attachWindowCaptures());

        actionButtonPanel.add(attachButton);
        actionButtonPanel.add(screenshotButton);
        actionButtonPanel.add(captureFramesButton);

        // Panel for send and run all buttons on the east
        JPanel eastButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 5, 0));
        eastButtonPanel.setOpaque(false);
        
        sendButton = new JButton("Send");
        sendButton.addActionListener(e -> sendMessage());
        
        stopButton = new JButton("Stop", IconUtils.getIcon("delete.png", 16, 16));
        stopButton.addActionListener(e -> chat.stop());
        stopButton.setVisible(false);

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
     * Reloads the panel with the new chat state.
     */
    public void reload() {
        this.chat = chatPanel.getChat();
        
        if (stagedListener != null) {
            stagedListener.unbind();
        }
        this.stagedListener = new EdtPropertyChangeListener(this, chat, "stagedUserMessage", evt -> updateStagedMessageUI());
        
        if (statusListener != null) {
            statusListener.unbind();
        }
        this.statusListener = new EdtPropertyChangeListener(this, chat.getStatusManager(), "currentStatus", evt -> updateSendButtonState());

        resetMessage();
        updateStagedMessageUI();
        updateSendButtonState();
    }

    /**
     * Updates the underlying {@code currentMessage} model with the current text
     * from the input area. The UI updates automatically via property change listeners.
     */
    private void updateMessageText() {
        currentMessage.setText(inputTextArea.getText());
        updateSendButtonState();
    }

    /**
     * Attaches a single file path to the current message.
     *
     * @param p The path to attach.
     * @throws Exception if the attachment fails.
     */
    void attach(Path p) throws Exception {
        currentMessage.addAttachment(p);
        updateSendButtonState();
    }

    /**
     * Opens a file chooser and attaches the selected files to the current message.
     */
    private void attachFiles() {
        JFileChooser fileChooser = new JFileChooser();
        fileChooser.setMultiSelectionEnabled(true);
        int result = fileChooser.showOpenDialog(this);

        if (result == JFileChooser.APPROVE_OPTION) {
            File[] selectedFiles = fileChooser.getSelectedFiles();

            executeTask(
                    "Attach Files",
                    () -> {
                        List<Path> paths = Arrays.stream(selectedFiles)
                                .map(File::toPath)
                                .collect(Collectors.toList());
                        currentMessage.addAttachments(paths);
                        return null;
                    }
            );
        }
    }

    /**
     * Captures screenshots of all screen devices and attaches them to the current message.
     */
    private void attachScreenshot() {
        executeTask(
                "Attach Screenshot",
                () -> {
                    List<Path> files = UICapture.screenshotAllScreenDevices();
                    currentMessage.addAttachments(files);
                    return null;
                }
        );
    }

    /**
     * Captures screenshots of all application frames and attaches them to the current message.
     */
    private void attachWindowCaptures() {
        executeTask(
                "Attach Application Frames",
                () -> {
                    List<Path> files = UICapture.screenshotAllJFrames();
                    currentMessage.addAttachments(files);
                    return null;
                }
        );
    }

    /**
     * Sends the {@code currentMessage} to the chat asynchronously.
     */
    private void sendMessage() {
        setButtonsEnabled(false);

        final InputUserMessage messageToSend = this.currentMessage; 
        
        // IMMEDIATE CLEAR
        resetMessage();
        
        executeTask(
                "Send Message",
                () -> {
                    chat.sendMessage(messageToSend);
                    return null;
                },
                (result) -> {
                    // On Success (UI Thread)
                    setButtonsEnabled(true);
                    SwingUtilities.invokeLater(() -> inputTextArea.requestFocusInWindow());
                },
                (error) -> {
                    // On Error (UI Thread)
                    setButtonsEnabled(true);
                }
        );
    }

    private void updateStagedMessageUI() {
        InputUserMessage staged = chat.getStagedUserMessage();
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
        chatPanel.revalidate();
        chatPanel.repaint();
    }

    private void updateSendButtonState() {
        ChatStatus status = chat.getStatusManager().getCurrentStatus();
        boolean isApiActive = status == ChatStatus.API_CALL_IN_PROGRESS || status == ChatStatus.WAITING_WITH_BACKOFF;
        
        stopButton.setVisible(isApiActive);
        stopButton.setEnabled(isApiActive);
        
        // Send button is enabled unless we are waiting for a candidate choice.
        boolean canSend = status != ChatStatus.CANDIDATE_CHOICE_PROMPT;
        
        // If visible history is empty and current message is empty, we can't send anything.
        if (currentMessage.isEmpty() && chat.getContextManager().buildVisibleHistory().isEmpty()) {
            canSend = false;
        }
        
        sendButton.setEnabled(canSend);
    }

    private void revertStagedMessage() {
        InputUserMessage staged = chat.getStagedUserMessage();
        if (staged != null) {
            chat.setStagedUserMessage(null);
            
            this.currentMessage = staged;
            inputTextArea.setText(staged.getText());
            
            // Re-render preview
            UserInputMessagePanel newRenderer = new UserInputMessagePanel(chatPanel, this.currentMessage);
            previewScrollPane.setViewportView(newRenderer);
            this.inputMessagePreview = newRenderer;
            updateSendButtonState();
            SwingUtilities.invokeLater(() -> inputTextArea.requestFocusInWindow());
        }
    }

    private void deleteStagedMessage() {
        chat.setStagedUserMessage(null);
    }

    /**
     * Helper method to execute a SwingTask with common success/error handling.
     *
     * @param <T> The result type of the task.
     * @param taskName The name of the task.
     * @param backgroundTask The task to execute.
     */
    private <T> void executeTask(String taskName, Callable<T> backgroundTask) {
        new SwingTask<>(this, taskName, backgroundTask).execute();
    }

    /**
     * Helper method to execute a SwingTask with common success/error handling.
     *
     * @param <T> The result type of the task.
     * @param taskName The name of the task.
     * @param backgroundTask The task to execute.
     * @param onDone The consumer for the task result.
     * @param onError The consumer for any exception.
     */
    private <T> void executeTask(String taskName, Callable<T> backgroundTask, Consumer<T> onDone, Consumer<Exception> onError) {
        new SwingTask<>(
                this, 
                taskName,
                backgroundTask,
                onDone,
                onError
        ).execute();
    }

    /**
     * Resets the input field and creates a new, empty {@link InputUserMessage}
     * for the next turn.
     */
    private void resetMessage() {
        // CRITICAL FIX: Swap the currentMessage object BEFORE clearing the text area.
        // This prevents the DocumentListener from clearing the text of the message
        // that was just sent and added to the history.
        this.currentMessage = new InputUserMessage(chat);
        
        inputTextArea.setText("");
        
        // Recreate the renderer for the new message.
        // The old renderer's EdtPropertyChangeListener will unbind automatically
        // when it is removed from the scroll pane's viewport.
        UserInputMessagePanel newRenderer = new UserInputMessagePanel(chatPanel, this.currentMessage);
        previewScrollPane.setViewportView(newRenderer);
        this.inputMessagePreview = newRenderer;
        updateSendButtonState();
    }

    /**
     * Enables or disables all input buttons.
     * @param enabled True to enable, false to disable.
     */
    private void setButtonsEnabled(boolean enabled) {
        // sendButton is now managed by updateSendButtonState
        attachButton.setEnabled(enabled);
        screenshotButton.setEnabled(enabled);
        captureFramesButton.setEnabled(enabled);
        microphonePanel.setMicrophoneComponentsEnabled(enabled);
    }
}
