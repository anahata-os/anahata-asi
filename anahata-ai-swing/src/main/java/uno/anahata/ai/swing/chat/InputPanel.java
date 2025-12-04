/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.JXTextArea;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.core.InputUserMessage;
import uno.anahata.ai.swing.icons.IconUtils;
import uno.anahata.ai.swing.icons.MicrophoneIcon;
import uno.anahata.ai.swing.icons.RecordingIcon;
import uno.anahata.ai.swing.internal.SwingTask;

/**
 * A fully functional and responsive user input component for the V2 chat.
 * <p>
 * This panel manages a "live" {@link InputUserMessage} object that is updated in
 * real-time as the user types. It uses a {@link SwingTask} to send messages
 * asynchronously, ensuring the UI never freezes during API calls.
 *
 * @author pablo
 */
@Slf4j
@Getter
public class InputPanel extends JPanel {

    private final Chat chat;

    // UI Components
    private JXTextArea inputTextArea;
    private JButton sendButton;
    private JToggleButton micButton;
    private JButton attachButton;
    private JButton screenshotButton;
    private JButton captureFramesButton;

    /**
     * The "live" message being composed by the user. This is the single source
     * of truth for the current input.
     */
    private InputUserMessage currentMessage;

    public InputPanel(Chat chat) {
        super(new BorderLayout(5, 5));
        this.chat = chat;
        resetMessage();
        initComponents();
    }

    private void initComponents() {
        setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        inputTextArea = new JXTextArea("Type your message here (Ctrl+Enter to send)");
        inputTextArea.setLineWrap(true);
        inputTextArea.setWrapStyleWord(true);

        // --- REAL-TIME MODEL UPDATE ---
        // Listen for changes in the text area and update the live message model instantly.
        inputTextArea.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                updateMessageText();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                updateMessageText();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                updateMessageText();
            }
        });

        // Ctrl+Enter to send
        KeyStroke ctrlEnter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, KeyEvent.CTRL_DOWN_MASK);
        inputTextArea.getInputMap(JComponent.WHEN_FOCUSED).put(ctrlEnter, "sendMessage");
        inputTextArea.getActionMap().put("sendMessage", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                sendMessage();
            }
        });

        JScrollPane inputScrollPane = new JScrollPane(inputTextArea);
        add(inputScrollPane, BorderLayout.CENTER);

        // Panel for buttons on the south side
        JPanel southButtonPanel = new JPanel(new BorderLayout(5, 0));

        // Panel for action buttons (mic, attach, etc.) on the west
        JPanel actionButtonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));

        micButton = new JToggleButton(new MicrophoneIcon(24));
        micButton.setSelectedIcon(new RecordingIcon(24));
        micButton.setToolTipText("Click to start/stop recording");

        attachButton = new JButton(IconUtils.getIcon("attach.png"));
        attachButton.setToolTipText("Attach Files");

        screenshotButton = new JButton(IconUtils.getIcon("desktop_screenshot.png"));
        screenshotButton.setToolTipText("Attach Desktop Screenshot");

        captureFramesButton = new JButton(IconUtils.getIcon("capture_frames.png"));
        captureFramesButton.setToolTipText("Attach Application Frames");

        actionButtonPanel.add(micButton);
        actionButtonPanel.add(attachButton);
        actionButtonPanel.add(screenshotButton);
        actionButtonPanel.add(captureFramesButton);

        sendButton = new JButton("Send");
        sendButton.addActionListener(e -> sendMessage());

        southButtonPanel.add(actionButtonPanel, BorderLayout.WEST);
        southButtonPanel.add(sendButton, BorderLayout.EAST);

        add(southButtonPanel, BorderLayout.SOUTH);
    }

    /**
     * Updates the underlying {@code currentMessage} model with the current text
     * from the input area.
     */
    private void updateMessageText() {
        currentMessage.setText(inputTextArea.getText());
    }

    /**
     * Sends the {@code currentMessage} to the chat asynchronously.
     */
    private void sendMessage() {
        if (currentMessage.isEmpty()) {
            return; // Don't send empty messages
        }

        final InputUserMessage messageToSend = this.currentMessage;
        final String textToRestoreOnError = inputTextArea.getText(); // Keep a copy
        
        // --- UX IMPROVEMENT ---
        // Clear the input immediately and create a new message for the next turn.
        resetMessage();
        // --------------------

        setButtonsEnabled(false);

        // Use the generic SwingTask to run the blocking chat method in the background.
        SwingTask.run(
            () -> {
                chat.sendMessage(messageToSend);
                return null; // Return Void
            },
            (result) -> {
                // On Success (UI Thread)
                log.info("Message sent successfully.");
                setButtonsEnabled(true);
                inputTextArea.requestFocusInWindow();
            },
            (error) -> {
                // On Error (UI Thread)
                log.error("Failed to send message", error);
                JOptionPane.showMessageDialog(this,
                                              "An error occurred: " + error.getMessage(),
                                              "Error",
                                              JOptionPane.ERROR_MESSAGE);
                // Restore UI state
                setButtonsEnabled(true);
                inputTextArea.setText(textToRestoreOnError); // Restore the text
                this.currentMessage = messageToSend; // Restore the message with attachments
            }
        );
    }
    
    /**
     * Resets the input field and creates a new, empty {@link InputUserMessage}
     * for the next turn.
     */
    private void resetMessage() {
        inputTextArea.setText("");
        this.currentMessage = new InputUserMessage(chat);
    }

    private void setButtonsEnabled(boolean enabled) {
        sendButton.setEnabled(enabled);
        micButton.setEnabled(enabled);
        attachButton.setEnabled(enabled);
        screenshotButton.setEnabled(enabled);
        captureFramesButton.setEnabled(enabled);
    }
}
