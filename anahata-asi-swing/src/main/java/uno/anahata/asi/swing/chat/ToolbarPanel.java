/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat;

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
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.chat.ChatConfig;
import uno.anahata.asi.swing.icons.AutoReplyIcon;
import uno.anahata.asi.swing.icons.CompressIcon;
import uno.anahata.asi.swing.icons.LocalToolsIcon;
import uno.anahata.asi.swing.icons.PrunedPartsIcon;
import uno.anahata.asi.swing.icons.RestartIcon;
import uno.anahata.asi.swing.icons.ServerToolsIcon;

/**
 * The vertical toolbar panel for the chat UI, containing primary action toggles.
 *
 * @author anahata
 */
@Slf4j
@Getter
public class ToolbarPanel extends JPanel {
    /** The size of the icons in the toolbar. */
    private static final int ICON_SIZE = 24;

    /** The parent chat panel. */
    private final ChatPanel chatPanel; 
    /** The active chat session. */
    private Chat chat;
    /** The chat configuration. */
    private SwingChatConfig config;
    
    /** Toggle button for local tool execution. */
    private JToggleButton toggleLocalToolsButton;
    /** Toggle button for server-side tool execution. */
    private JToggleButton toggleServerToolsButton;
    /** Toggle button for automatic tool loop replies. */
    private JToggleButton toggleAutoreplyButton;
    /** Toggle button for showing/hiding pruned parts. */
    private JToggleButton togglePrunedPartsButton;
    /** Button to clear the chat history. */
    private JButton clearChatButton;
    /** Button to trigger context compression. */
    private JButton compressContextButton;

    /**
     * Constructs a new ToolbarPanel.
     * 
     * @param chatPanel The parent chat panel.
     */
    public ToolbarPanel(ChatPanel chatPanel) {
        this.chatPanel = chatPanel;
        this.chat = chatPanel.getChat();
        this.config = chatPanel.getChatConfig();
        
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    }

    /**
     * Initializes the UI components and layout.
     */
    public void initComponents() {
        // 1. Clear Chat Button (Top)
        clearChatButton = createIconButton(new RestartIcon(ICON_SIZE), "Clear the entire chat history.");
        clearChatButton.addActionListener(this::clearChat);
        add(clearChatButton);

        // 2. Compress Context Button (Top)
        compressContextButton = createIconButton(new CompressIcon(ICON_SIZE), "Compress the context.");
        compressContextButton.addActionListener(this::compressContext);
        // TODO: This needs to trigger a special, one-off API call that only includes the ContextWindow tools.
        // This requires a new mechanism in the Chat orchestrator.
        compressContextButton.setEnabled(false);
        add(compressContextButton);

        // Vertical Glue to push toggles to the middle
        add(Box.createVerticalGlue());
        
        // 3. Toggle Pruned Parts Button (Middle)
        togglePrunedPartsButton = createIconToggleButton(new PrunedPartsIcon(ICON_SIZE), "Show/Hide pruned parts in the conversation view.", config.isShowPrunedParts());
        togglePrunedPartsButton.addActionListener(this::togglePrunedParts);
        add(togglePrunedPartsButton);

        // 4. Toggle Local Tools Button (Middle)
        toggleLocalToolsButton = createIconToggleButton(new LocalToolsIcon(ICON_SIZE), "Enable/Disable local tool execution (Functions).", config.isLocalToolsEnabled());
        toggleLocalToolsButton.addActionListener(this::toggleLocalTools);
        add(toggleLocalToolsButton);

        // 5. Toggle Server Tools Button (Middle)
        toggleServerToolsButton = createIconToggleButton(new ServerToolsIcon(ICON_SIZE), "Enable/Disable server-side tool execution (e.g., Google Search).", config.isServerToolsEnabled());
        toggleServerToolsButton.addActionListener(this::toggleServerTools);
        add(toggleServerToolsButton);
        
        // 6. Toggle Autoreply Button (Middle)
        toggleAutoreplyButton = createIconToggleButton(new AutoReplyIcon(ICON_SIZE), "Enable/Disable automatic replying of tool execution.", config.isAutoReplyTools());
        toggleAutoreplyButton.addActionListener(this::toggleAutoreply);
        add(toggleAutoreplyButton);
        
        // Vertical Glue to keep the toggles in the middle
        add(Box.createVerticalGlue());

        // Initial state sync
        syncToggles();
    }

    /**
     * Reloads the panel with the new chat state.
     */
    public void reload() {
        this.chat = chatPanel.getChat();
        this.config = chatPanel.getChatConfig();
        
        togglePrunedPartsButton.setSelected(config.isShowPrunedParts());
        toggleAutoreplyButton.setSelected(config.isAutoReplyTools());
        syncToggles();
    }

    /**
     * Helper method to create a standard icon button.
     * 
     * @param icon The icon to display.
     * @param tooltip The tooltip text.
     * @return The created JButton.
     */
    private JButton createIconButton(javax.swing.Icon icon, String tooltip) {
        JButton button = new JButton(icon);
        button.setToolTipText(tooltip);
        button.setAlignmentX(CENTER_ALIGNMENT);
        button.setMaximumSize(new Dimension(Integer.MAX_VALUE, button.getPreferredSize().height));
        return button;
    }

    /**
     * Helper method to create a standard icon toggle button.
     * 
     * @param icon The icon to display.
     * @param tooltip The tooltip text.
     * @param selected The initial selected state.
     * @return The created JToggleButton.
     */
    private JToggleButton createIconToggleButton(javax.swing.Icon icon, String tooltip, boolean selected) {
        JToggleButton button = new JToggleButton(icon, selected);
        button.setToolTipText(tooltip);
        button.setAlignmentX(CENTER_ALIGNMENT);
        button.setMaximumSize(new Dimension(Integer.MAX_VALUE, button.getPreferredSize().height));
        return button;
    }

    /**
     * Action listener for the clear chat button.
     * @param e The action event.
     */
    private void clearChat(ActionEvent e) {
        log.info("Clear Chat button pressed.");
        chat.clear();
    }

    /**
     * Action listener for the compress context button.
     * @param e The action event.
     */
    private void compressContext(ActionEvent e) {
        log.info("Compress Context button pressed. Action is currently disabled.");
    }
    
    /**
     * Action listener for the toggle pruned parts button.
     * @param e The action event.
     */
    private void togglePrunedParts(ActionEvent e) {
        boolean show = togglePrunedPartsButton.isSelected();
        config.setShowPrunedParts(show);
        log.info("Show Pruned Parts toggled to: {}", show);
        // TODO: Trigger a full re-render of the conversation view here
    }

    /**
     * Action listener for the toggle local tools button.
     * @param e The action event.
     */
    private void toggleLocalTools(ActionEvent e) {
        boolean selected = toggleLocalToolsButton.isSelected();
        config.setLocalToolsEnabled(selected);
        log.info("Local tools toggled to: {}", selected);
        syncToggles();
    }

    /**
     * Action listener for the toggle server tools button.
     * @param e The action event.
     */
    private void toggleServerTools(ActionEvent e) {
        boolean selected = toggleServerToolsButton.isSelected();
        config.setServerToolsEnabled(selected);
        log.info("Server tools toggled to: {}", selected);
        syncToggles();
    }
    
    /**
     * Synchronizes the toggle buttons with the current configuration.
     */
    private void syncToggles() {
        toggleLocalToolsButton.setSelected(config.isLocalToolsEnabled());
        toggleServerToolsButton.setSelected(config.isServerToolsEnabled());
    }

    /**
     * Action listener for the toggle autoreply button.
     * @param e The action event.
     */
    private void toggleAutoreply(ActionEvent e) {
        boolean enabled = toggleAutoreplyButton.isSelected();
        chat.getConfig().setAutoReplyTools(enabled);
        log.info("Auto-Reply after tool execution toggled to: {}", enabled);
    }
}
