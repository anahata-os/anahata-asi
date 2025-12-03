/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details.
 */
package uno.anahata.ai.swing.chat;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.chat.ChatConfig;
import uno.anahata.ai.swing.AutoReplyIcon;
import uno.anahata.ai.swing.CompressIcon;
import uno.anahata.ai.swing.LocalToolsIcon;
import uno.anahata.ai.swing.RestartIcon;
import uno.anahata.ai.swing.ServerToolsIcon;

/**
 * The vertical toolbar panel for the chat UI, containing primary action toggles.
 *
 * @author pablo
 */
@Slf4j
@Getter
public class ToolbarPanel extends JPanel {
    private static final int ICON_SIZE = 24;

    private final Chat chat;
    private JToggleButton toggleLocalToolsButton;
    private JToggleButton toggleServerToolsButton;
    private JToggleButton toggleAutoreplyButton;
    private JButton clearChatButton;
    private JButton compressContextButton;

    public ToolbarPanel(Chat chat) {
        this.chat = chat;
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    }

    public void initComponents() {
        ChatConfig config = chat.getConfig();

        // 1. Clear Chat Button (Top)
        clearChatButton = createIconButton(new RestartIcon(ICON_SIZE), "Clear the entire chat history.");
        clearChatButton.addActionListener(this::clearChat);
        add(clearChatButton);

        // 2. Compress Context Button (Top)
        compressContextButton = createIconButton(new CompressIcon(ICON_SIZE), "Prune all stale and ephemeral resources from the context window.");
        compressContextButton.addActionListener(this::compressContext);
        // TODO: This needs to trigger a special, one-off API call that only includes the ContextWindow tools.
        // This requires a new mechanism in the Chat orchestrator.
        compressContextButton.setEnabled(false);
        add(compressContextButton);

        // Vertical Glue to push toggles to the bottom
        add(Box.createVerticalGlue());

        // 3. Toggle Local Tools Button (Bottom)
        toggleLocalToolsButton = createIconToggleButton(new LocalToolsIcon(ICON_SIZE), "Enable/Disable local tool execution (Functions).", config.isLocalToolsEnabled());
        toggleLocalToolsButton.addActionListener(this::toggleLocalTools);
        add(toggleLocalToolsButton);

        // 4. Toggle Server Tools Button (Bottom)
        toggleServerToolsButton = createIconToggleButton(new ServerToolsIcon(ICON_SIZE), "Enable/Disable server-side tool execution (e.g., Google Search).", !config.isLocalToolsEnabled());
        toggleServerToolsButton.addActionListener(this::toggleServerTools);
        add(toggleServerToolsButton);
        
        // 5. Toggle Autoreply Button (Bottom)
        toggleAutoreplyButton = createIconToggleButton(new AutoReplyIcon(ICON_SIZE), "Enable/Disable automatic re-prompting after tool execution.", config.isAutoReplyTools());
        toggleAutoreplyButton.addActionListener(this::toggleAutoreply);
        add(toggleAutoreplyButton);
        
        // Initial state sync
        updateToolToggles(config.isLocalToolsEnabled());
    }

    private JButton createIconButton(javax.swing.Icon icon, String tooltip) {
        JButton button = new JButton(icon);
        button.setToolTipText(tooltip);
        button.setAlignmentX(CENTER_ALIGNMENT);
        button.setMaximumSize(new Dimension(Integer.MAX_VALUE, button.getPreferredSize().height));
        return button;
    }

    private JToggleButton createIconToggleButton(javax.swing.Icon icon, String tooltip, boolean selected) {
        JToggleButton button = new JToggleButton(icon, selected);
        button.setToolTipText(tooltip);
        button.setAlignmentX(CENTER_ALIGNMENT);
        button.setMaximumSize(new Dimension(Integer.MAX_VALUE, button.getPreferredSize().height));
        return button;
    }

    private void clearChat(ActionEvent e) {
        log.info("Clear Chat button pressed.");
        chat.clear();
    }

    private void compressContext(ActionEvent e) {
        log.info("Compress Context button pressed. Action is currently disabled.");
    }

    private void toggleLocalTools(ActionEvent e) {
        updateToolToggles(true);
    }

    private void toggleServerTools(ActionEvent e) {
        updateToolToggles(false);
    }
    
    private void updateToolToggles(boolean localEnabled) {
        chat.getConfig().setLocalToolsEnabled(localEnabled);
        toggleLocalToolsButton.setSelected(localEnabled);
        toggleServerToolsButton.setSelected(!localEnabled);
        log.info("Tool execution mode set to: {}", localEnabled ? "LOCAL" : "SERVER");
    }

    private void toggleAutoreply(ActionEvent e) {
        boolean enabled = toggleAutoreplyButton.isSelected();
        chat.getConfig().setAutoReplyTools(enabled);
        log.info("Auto-Reply after tool execution toggled to: {}", enabled);
    }
}
