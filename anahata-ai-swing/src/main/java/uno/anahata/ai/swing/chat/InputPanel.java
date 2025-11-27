/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.Dimension;
import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import lombok.Getter;
import uno.anahata.ai.chat.Chat;

/**
 * A placeholder for the user input component.
 *
 * @author pablo
 */
@Getter
public class InputPanel extends JPanel {
    private final Chat chat;

    public InputPanel(Chat chat) {
        this.chat = chat;
        // A simple text area as a placeholder for now.
        JTextArea textArea = new JTextArea("InputPanel Placeholder");
        textArea.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
        add(textArea);
        setPreferredSize(new Dimension(getPreferredSize().width, 150));
    }
}
