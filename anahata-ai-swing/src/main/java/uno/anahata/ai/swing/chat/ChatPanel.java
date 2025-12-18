/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.BorderLayout;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import lombok.Getter;
import lombok.NonNull;
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

    /** The chat session orchestrator. */
    private final Chat chat; 
    /** The chat configuration. */
    private final SwingChatConfig chatConfig; 
    /** The tabbed pane for switching between chat and tools. */
    private final JTabbedPane tabbedPane;
    /** The panel for managing tools. */
    private final ToolsPanel toolsPanel;
    /** The panel for user input. */
    private final InputPanel inputPanel;
    /** The header panel. */
    private final HeaderPanel headerPanel;
    /** The toolbar panel. */
    private final ToolbarPanel toolbarPanel;
    /** The status panel. */
    private final StatusPanel statusPanel; 
    /** The main conversation view. */
    private final ConversationPanel conversationPanel; 

    /**
     * Constructs a new ChatPanel.
     *
     * @param chat The chat session to associate with this panel.
     */
    public ChatPanel(@NonNull Chat chat) { 
        this.chat = chat; 
        this.chatConfig = (SwingChatConfig) chat.getConfig(); 
        
        this.tabbedPane = new JTabbedPane();
        this.toolsPanel = new ToolsPanel(chat);
        this.inputPanel = new InputPanel(this); 
        this.headerPanel = new HeaderPanel(chat);
        this.toolbarPanel = new ToolbarPanel(this); 
        this.statusPanel = new StatusPanel(this); 
        this.conversationPanel = new ConversationPanel(this); 
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
        southPanel.add(inputPanel, BorderLayout.NORTH); 
        southPanel.add(statusPanel, BorderLayout.SOUTH); 

        // Add components to the main panel
        add(headerPanel, BorderLayout.NORTH);
        add(toolbarPanel, BorderLayout.WEST);
        add(tabbedPane, BorderLayout.CENTER);
        add(southPanel, BorderLayout.SOUTH); 
    }
    
    /**
     * Convenience method to get the EditorKitProvider from the chat configuration.
     * @return The EditorKitProvider.
     */
    public EditorKitProvider getEditorKitProvider() {
        return chatConfig.getEditorKitProvider();
    }
}
