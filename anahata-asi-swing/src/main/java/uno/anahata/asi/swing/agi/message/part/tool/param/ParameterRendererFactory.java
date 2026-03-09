/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.TextUtils;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * A factory for creating specialized {@link ParameterRenderer} instances.
 * <p>
 * This factory maintains a static registry of value types and uses semantic 
 * probers to decide the best visual representation for a given tool parameter.
 * </p>
 * <p>
 * <b>Representational Fidelity:</b> This factory authoritatively prefers raw 
 * Java representation via {@link TextUtils#resolveContentString} over JSON 
 * for all parameter types to ensure human-readability in the UI.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class ParameterRendererFactory {

    /** Static registry mapping value types to their specialized renderer classes. */
    private static final Map<Class<?>, Class<? extends ParameterRenderer<?>>> REGISTRY = new ConcurrentHashMap<>();

    /**
     * Registers a specialized renderer class for a specific parameter value type.
     * @param type The class of the value (e.g., FullTextFileCreate.class).
     * @param rendererClass The class of the renderer (e.g., FullTextFileCreateRenderer.class).
     */
    public static void register(Class<?> type, Class<? extends ParameterRenderer<?>> rendererClass) {
        REGISTRY.put(type, rendererClass);
    }

    /**
     * Unified creation logic for parameter renderers.
     * <p>
     * <b>High-Fidelity Strategy:</b> This method attempts to resolve a 
     * specialized renderer from the registry, but authoritatively falls back 
     * to a {@link ObjectToStringParameterRenderer} for all other types.
     * </p>
     * <p>
     * <b>Semantic Bridge:</b> It converts complex types (Enums, Arrays) to 
     * high-fidelity Strings before passing them to the code block renderer 
     * to prevent bridge-method ClassCastExceptions.
     * </p>
     * 
     * @param agiPanel The parent agi panel.
     * @param call The tool call.
     * @param paramName The parameter name.
     * @param value The current value.
     * @param rendererId Optional renderer hint (e.g., "java", "json").
     * @return A specialized or fallback renderer.
     */
    public static ParameterRenderer<?> create(AgiPanel agiPanel, AbstractToolCall<?, ?> call, String paramName, Object value, String rendererId) {
        // 1. Check for Specialized Registry Hits
        if (value != null) {
            Class<? extends ParameterRenderer<?>> rendererClass = REGISTRY.get(value.getClass());
            if (rendererClass != null) {
                try {
                    ParameterRenderer renderer = rendererClass.getDeclaredConstructor().newInstance();
                    renderer.init(agiPanel, call, paramName, value);
                    return renderer;
                } catch (Exception e) {
                    log.error("Failed to instantiate specialized renderer: {}", rendererClass.getName(), e);
                }
            }
        }

        // 2. Authoritative Fallback: High-Fidelity Object-to-String Renderer
        String lang = (rendererId != null && !rendererId.isEmpty()) ? rendererId : "text";

        ObjectToStringParameterRenderer renderer = new ObjectToStringParameterRenderer();
        renderer.setLanguage(lang);
        renderer.setEditable(value instanceof String); // Policy: Only pure Strings are editable for now
        renderer.init(agiPanel, call, paramName, value);
        
        return renderer;
    }
}
