/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.chat.render;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * A factory for creating specialized {@link ParameterRenderer} instances.
 * It maintains a static registry of value types to renderer classes.
 * 
 * @author anahata
 */
@Slf4j
public class ParameterRendererFactory {

    /** Static registry mapping value types to their specialized renderer classes. */
    private static final Map<Class<?>, Class<? extends ParameterRenderer<?>>> REGISTRY = new ConcurrentHashMap<>();

    /**
     * Registers a specialized renderer class for a specific parameter value type.
     * @param type The class of the value (e.g., TextFileUpdate.class).
     * @param rendererClass The class of the renderer (e.g., TextFileUpdateRenderer.class).
     */
    public static void register(Class<?> type, Class<? extends ParameterRenderer<?>> rendererClass) {
        REGISTRY.put(type, rendererClass);
    }

    /**
     * Unified creation logic for parameter renderers.
     * 
     * @param chatPanel The parent chat panel.
     * @param call The tool call.
     * @param paramName The parameter name.
     * @param value The current value.
     * @param rendererId Optional renderer hint (e.g., "java", "json").
     * @return A specialized or fallback renderer.
     */
    public static ParameterRenderer<?> create(ChatPanel chatPanel, AbstractToolCall<?, ?> call, String paramName, Object value, String rendererId) {
        ParameterRenderer renderer = null;

        // 1. Priority: rendererId (Explicit hints from @AiToolParam)
        if (rendererId != null && !rendererId.isEmpty()) {
            renderer = new CodeBlockParameterRenderer();
            String valStr = (value == null) ? "null" : (value instanceof String s) ? s : JacksonUtils.prettyPrint(value);
            // We pass rendererId as the value initially or handle it in init
            // For CodeBlockRenderer specifically, we use a specialized init or cast
            ((CodeBlockParameterRenderer)renderer).init(chatPanel, call, paramName, valStr, rendererId);
        }

        // 2. Registry: Specialized types (e.g., TextFileUpdate)
        if (renderer == null && value != null) {
            Class<? extends ParameterRenderer<?>> rendererClass = REGISTRY.get(value.getClass());
            if (rendererClass != null) {
                try {
                    renderer = rendererClass.getDeclaredConstructor().newInstance();
                    renderer.init(chatPanel, call, paramName, value);
                } catch (Exception e) {
                    log.error("Failed to instantiate specialized renderer: {}", rendererClass.getName(), e);
                    renderer = null;
                }
            }
        }

        // 3. Fallback: Markup (Markdown/HTML)
        if (renderer == null) {
            renderer = new MarkupParameterRenderer();
            String valStr = (value == null) ? "null" : (value instanceof String s) ? s : JacksonUtils.prettyPrint(value);
            renderer.init(chatPanel, call, paramName, valStr);
        }

        return renderer;
    }
}
