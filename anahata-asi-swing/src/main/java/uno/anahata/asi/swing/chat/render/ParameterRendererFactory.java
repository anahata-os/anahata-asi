/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.chat.render;

import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * A functional interface for factories that create specialized {@link ParameterRenderer}
 * instances for tool parameters based on their Java type.
 * 
 * @author anahata
 */
@FunctionalInterface
public interface ParameterRendererFactory {

    /**
     * Creates a new renderer for a tool parameter.
     * 
     * @param chatPanel The parent chat panel.
     * @param call The tool call being rendered.
     * @param paramName The name of the parameter.
     * @param value The current value of the parameter.
     * @return A specialized renderer instance.
     */
    ParameterRenderer<?> create(ChatPanel chatPanel, AbstractToolCall<?, ?> call, String paramName, Object value);
}
