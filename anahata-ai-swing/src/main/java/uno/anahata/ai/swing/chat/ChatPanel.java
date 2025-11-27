/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.BorderLayout;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import lombok.Getter;
import uno.anahata.ai.chat.Chat;

/**
 * The main, top-level panel for the Anahata AI Swing UI.
 * This class acts as an aggregator for all other UI components (e.g., input panel, conversation view, status bar).
 *
 * @author pablo
 */
@Getter
public class ChatPanel extends JPanel {

    private final Chat chat;
    private final JTabbedPane tabbedPane;
    private final ToolsPanel toolsPanel;
    private final InputPanel inputPanel;
    private final JPanel conversationPanel; // Placeholder for the main chat view

    public ChatPanel(Chat chat) {
        this.chat = chat;
        this.tabbedPane = new JTabbedPane();
        this.toolsPanel = new ToolsPanel(chat);
        this.inputPanel = new InputPanel(chat);
        this.conversationPanel = new JPanel(); // Simple placeholder
    }

    /**
     * Initializes the components and layout of the panel.
     */
    public void initComponents() {
        setLayout(new BorderLayout(10, 10));

        // Initialize child components first
        toolsPanel.initComponents();

        // Configure Tabbed Pane
        tabbedPane.addTab("Chat", conversationPanel);
        tabbedPane.addTab("Tools", toolsPanel);

        // Add components to the main panel
        add(tabbedPane, BorderLayout.CENTER);
        add(inputPanel, BorderLayout.SOUTH);
    }
}
