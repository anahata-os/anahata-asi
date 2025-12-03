package uno.anahata.ai.swing.tools;

import javax.swing.JPanel;
import lombok.Getter;
import uno.anahata.ai.chat.Chat;

/**
 * A panel to display and manage available AI tools and their permissions.
 * This is the V2 replacement for the V1 FunctionsPanel.
 */
@Getter
public class ToolsPanel extends JPanel {
    private final Chat chat;

    public ToolsPanel(Chat chat) {
        this.chat = chat;
        initComponents();
    }

    private void initComponents() {
        // TODO: Implement the UI for displaying and managing tools.
        // This will be built out according to the user's detailed feedback.
    }
}
