/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat;

import java.awt.BorderLayout;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.swing.chat.render.CandidateSelectionPanel;

/**
 * The main, top-level panel for the Anahata AI Swing UI.
 * This class acts as an aggregator for all other UI components (e.g., input panel, conversation view, status bar).
 *
 * @author anahata
 */
@Getter
public class ChatPanel extends JPanel {

    /** The chat session orchestrator. */
    private Chat chat; 
    /** The chat configuration. */
    private SwingChatConfig chatConfig; 
    /** The tabbed pane for switching between chat and tools. */
    private final JTabbedPane tabbedPane;
    /** The panel for editing request configuration. */
    private final RequestConfigPanel configPanel;
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
    /** The panel for displaying and selecting response candidates. */
    private final CandidateSelectionPanel candidateSelectionPanel;

    /**
     * Constructs a new ChatPanel by creating a new Chat session with the provided configuration.
     *
     * @param config The chat configuration.
     */
    public ChatPanel(@NonNull SwingChatConfig config) { 
        this(new Chat(config));
    }

    /**
     * Constructs a new ChatPanel for an existing Chat session.
     * 
     * @param chat The existing chat session.
     */
    public ChatPanel(@NonNull Chat chat) {
        this.chat = chat;
        this.chatConfig = (SwingChatConfig) chat.getConfig();
        
        this.tabbedPane = new JTabbedPane();
        this.configPanel = new RequestConfigPanel(this);
        this.toolsPanel = new ToolsPanel(this);
        this.inputPanel = new InputPanel(this); 
        this.headerPanel = new HeaderPanel(this);
        this.toolbarPanel = new ToolbarPanel(this); 
        this.statusPanel = new StatusPanel(this); 
        this.conversationPanel = new ConversationPanel(this); 
        this.candidateSelectionPanel = new CandidateSelectionPanel(this);
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
        tabbedPane.addTab("Config", configPanel);
        tabbedPane.addTab("Tools", toolsPanel);

        // Create a panel to hold CandidateSelectionPanel, InputPanel and StatusPanel
        JPanel southPanel = new JPanel(new BorderLayout());
        // CandidateSelectionPanel sits between conversation and input.
        southPanel.add(candidateSelectionPanel, BorderLayout.NORTH);
        // Use CENTER for inputPanel so it grows vertically when the split pane is resized.
        southPanel.add(inputPanel, BorderLayout.CENTER); 
        southPanel.add(statusPanel, BorderLayout.SOUTH); 

        // Use a SplitPane for the main content and the input area
        JSplitPane mainSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, tabbedPane, southPanel);
        mainSplitPane.setResizeWeight(1.0); // Give all extra space to the tabbed pane
        mainSplitPane.setDividerLocation(0.7); // Initial balance
        mainSplitPane.setOneTouchExpandable(true);

        // Add components to the main panel
        add(headerPanel, BorderLayout.NORTH);
        add(toolbarPanel, BorderLayout.WEST);
        add(mainSplitPane, BorderLayout.CENTER);
    }
    
    /**
     * Reloads the entire UI with a new Chat instance.
     * This is used when loading a saved session.
     * 
     * @param newChat The new chat session to load.
     */
    public void reload(@NonNull Chat newChat) {
        SwingUtilities.invokeLater(() -> {
            this.chat = newChat;
            this.chatConfig = (SwingChatConfig) newChat.getConfig();
            
            // Update child components
            headerPanel.reload();
            conversationPanel.reload();
            // Note: RequestConfigPanel doesn't have a reload() yet, but it's initialized with chat
            toolsPanel.reload();
            statusPanel.reload();
            inputPanel.reload();
            toolbarPanel.reload();
            
            revalidate();
            repaint();
        });
    }
}
