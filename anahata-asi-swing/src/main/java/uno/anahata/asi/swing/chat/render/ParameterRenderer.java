/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.chat.render;

import javax.swing.JComponent;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.swing.chat.ChatPanel;

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
     * @param chatPanel The parent chat panel.
     * @param call The tool call being rendered.
     * @param paramName The name of the parameter.
     * @param value The initial value.
     */
    void init(ChatPanel chatPanel, AbstractToolCall<?, ?> call, String paramName, T value);

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
}
