/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import javax.swing.JComponent;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * A generic interface for rendering tool parameters. 
 * This can render anything from a code diff to a monkey in pajamas.
 * 
 * @param <T> The type of the parameter value this renderer handles.
 * @author anahata
 */
public interface ParameterRenderer<T> {

    /**
     * Initializes the renderer with its context.
     * 
     * @param agiPanel The parent agi panel.
     * @param call The tool call being rendered.
     * @param paramName The name of the parameter.
     * @param value The initial value.
     */
    void init(AgiPanel agiPanel, AbstractToolCall<?, ?> call, String paramName, T value);

    /**
     * Returns the Swing component that performs the rendering.
     * @return The rendering component.
     */
    JComponent getComponent();

    /**
     * Updates the renderer with a new value.
     * @param value The new value to render.
     */
    void updateContent(T value);

    /**
     * Triggers the rendering logic.
     * @return True if a visual update occurred.
     */
    boolean render();

    /**
     * Returns the enclosing composite parent renderer, or {@code null} if this is a top-level parameter renderer.
     * 
     * @return The parent renderer, or null.
     */
    default ParameterRenderer<?> getParentRenderer() {
        return null;
    }

    /**
     * Sets the enclosing composite parent renderer.
     * 
     * @param parentRenderer The parent renderer.
     */
    default void setParentRenderer(ParameterRenderer<?> parentRenderer) {
    }

    /**
     * Notifies this renderer that one of its child renderers has changed its value.
     * Default implementation bubbles the notification up to {@link #getParentRenderer()} if present.
     * 
     * @param child The child renderer that changed.
     * @param newChildValue The new value emitted by the child.
     */
    default void childValueChanged(ParameterRenderer<?> child, Object newChildValue) {
        if (getParentRenderer() != null) {
            getParentRenderer().childValueChanged(child, newChildValue);
        }
    }
}
