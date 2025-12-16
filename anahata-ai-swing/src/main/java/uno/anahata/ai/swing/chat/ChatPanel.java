/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.BorderLayout;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import lombok.Getter;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.swing.chat.render.editorkit.EditorKitProvider;

/**
 * The main, top-level panel for the Anahata AI Swing UI.
 * This class acts as an aggregator for all other UI components (e.g., input panel, conversation view, status bar).
 *
 * @author pablo
 */
@Getter
public class ChatPanel extends JPanel {

    private final Chat chat; // Now instantiated here
    private final SwingChatConfig chatConfig; // New field for config
    private final JTabbedPane tabbedPane;
    private final ToolsPanel toolsPanel;
    private final InputPanel inputPanel;
    private final HeaderPanel headerPanel;
    private final ToolbarPanel toolbarPanel;
    private final StatusPanel statusPanel; // New: StatusPanel
    private final JPanel conversationPanel; // Placeholder for the main chat view

    public ChatPanel(Chat chat) { // Changed constructor argument
        this.chat = chat; // Initialize Chat here
        this.chatConfig = (SwingChatConfig) chat.getConfig(); // Derive chatConfig
        
        this.tabbedPane = new JTabbedPane();
        this.toolsPanel = new ToolsPanel(chat);
        this.inputPanel = new InputPanel(this); // FIX: Pass 'this' (ChatPanel)
        this.headerPanel = new HeaderPanel(chat);
        this.toolbarPanel = new ToolbarPanel(this); // FIX: Pass 'this' (ChatPanel)
        this.statusPanel = new StatusPanel(this); // New: Instantiate StatusPanel
        this.conversationPanel = new JPanel(); // Simple placeholder
    }

    /**
     * Initializes the components and layout of the panel.
     */
    public void initComponents() {
        setLayout(new BorderLayout(10, 10));

        // Initialize child components first
        toolsPanel.initComponents();
        headerPanel.initComponents();
        toolbarPanel.initComponents();

        // Configure Tabbed Pane
        tabbedPane.addTab("Chat", conversationPanel);
        tabbedPane.addTab("Tools", toolsPanel);

        // Create a panel to hold both InputPanel and StatusPanel
        JPanel southPanel = new JPanel(new BorderLayout());
        southPanel.add(inputPanel, BorderLayout.NORTH); // InputPanel at the top of the bottom area
        southPanel.add(statusPanel, BorderLayout.SOUTH); // StatusPanel below the input

        // Add components to the main panel
        add(headerPanel, BorderLayout.NORTH);
        add(toolbarPanel, BorderLayout.WEST);
        add(tabbedPane, BorderLayout.CENTER);
        add(southPanel, BorderLayout.SOUTH); // Add the combined south panel
    }
    
    /**
     * Convenience method to get the EditorKitProvider from the chat configuration.
     * @return The EditorKitProvider.
     */
    public EditorKitProvider getEditorKitProvider() {
        return chatConfig.getEditorKitProvider();
    }
}