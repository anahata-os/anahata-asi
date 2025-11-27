/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import lombok.Getter;
import uno.anahata.ai.chat.Chat;

/**
 * The vertical toolbar panel for the chat UI, containing primary action toggles.
 *
 * @author pablo
 */
@Getter
public class ToolbarPanel extends JPanel {
    private final Chat chat;

    public ToolbarPanel(Chat chat) {
        this.chat = chat;
        // A simple label as a placeholder for now.
        add(new JLabel("ToolbarPanel Placeholder"));
        setBorder(BorderFactory.createEtchedBorder());
    }
    
    public void initComponents() {
        // Component initialization will be done here in a future step.
    }
}
