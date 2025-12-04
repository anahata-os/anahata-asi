package uno.anahata.ai.swing.chat.render;

import java.awt.BorderLayout;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.TextPart;

/**
 * A simple renderer for {@link TextPart} content.
 *
 * @author Anahata
 */
public class TextPartRenderer implements PartRenderer {

    @Override
    public JComponent render(AbstractPart part) {
        TextPart textPart = (TextPart) part;
        // For now, we'll use a simple JEditorPane. We will add Markdown support later.
        JEditorPane editorPane = new JEditorPane("text/plain", textPart.getText());
        editorPane.setEditable(false);
        editorPane.setOpaque(false);
        
        // Wrap in a JPanel with BorderLayout to help with layout and wrapping.
        JPanel wrapper = new JPanel(new BorderLayout());
        wrapper.setOpaque(false);
        wrapper.add(editorPane, BorderLayout.CENTER);
        return wrapper;
    }
}
