package uno.anahata.ai.swing.chat.render;

import javax.swing.JComponent;
import uno.anahata.ai.model.core.AbstractPart;

/**
 * An interface for rendering a specific {@link AbstractPart} of a message
 * into a JComponent. This is the base for the V2 Swing rendering pipeline.
 *
 * @author Anahata
 */
public interface PartRenderer {

    /**
     * Renders a given Part into a JComponent.
     *
     * @param part The Part to render.
     * @return A JComponent representing the rendered Part.
     */
    JComponent render(AbstractPart part);

}
