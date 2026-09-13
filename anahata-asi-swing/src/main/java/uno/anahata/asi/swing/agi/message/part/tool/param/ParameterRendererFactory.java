/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.Displayable;
import uno.anahata.asi.internal.TextUtils;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.toolkit.java.AgiClassSource;
import uno.anahata.asi.toolkit.resources.text.FullTextFileCreate;

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

    /** Static registry mapping string IDs to their specialized renderer classes. */
    private static final Map<String, Class<? extends ParameterRenderer<?>>> ID_REGISTRY = new ConcurrentHashMap<>();
    

    /**
     * Registers a specialized renderer class for a specific parameter value type.
     * @param type The class of the value (e.g., FullTextFileCreate.class).
     * @param rendererClass The class of the renderer (e.g., FullTextFileCreateRenderer.class).
     */
    public static void register(Class<?> type, Class<? extends ParameterRenderer<?>> rendererClass) {
        REGISTRY.put(type, rendererClass);
    }

    /**
     * Registers a specialized renderer class for a specific String ID.
     * @param id The string ID (e.g., "java").
     * @param rendererClass The class of the renderer.
     */
    public static void registerById(String id, Class<? extends ParameterRenderer<?>> rendererClass) {
        if (id != null) {
            ID_REGISTRY.put(id.toLowerCase(), rendererClass);
        } else {
            throw new IllegalArgumentException("Id cannot be null");
        }
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
        String token0 = null;
        String token1 = null;

        if (rendererId != null && !rendererId.isBlank()) {
            String[] tokens = rendererId.split(",");
            token0 = tokens[0].trim().toLowerCase();
            if (tokens.length > 1 && !tokens[1].trim().isBlank()) {
                token1 = tokens[1].trim().toLowerCase();
            }
        }

        // 1. Collection / List Handling
        if (value instanceof List<?> list) {
            Class<? extends ParameterRenderer<?>> containerClass = null;
            String itemRendererId = null;

            if (token0 != null) {
                Class<? extends ParameterRenderer<?>> clazz0 = ID_REGISTRY.get(token0);
                if (clazz0 != null && AbstractListParameterRenderer.class.isAssignableFrom(clazz0)) {
                    // token0 is explicitly registered as a list container (e.g. tabs, vbox, wrap)
                    containerClass = clazz0;
                    itemRendererId = token1;
                } else {
                    // token0 is an item renderer (e.g. uri, java, resource_uuid)
                    itemRendererId = token0;
                    if (token1 != null) {
                        Class<? extends ParameterRenderer<?>> clazz1 = ID_REGISTRY.get(token1);
                        if (clazz1 != null && AbstractListParameterRenderer.class.isAssignableFrom(clazz1)) {
                            containerClass = clazz1;
                        }
                    }
                }
            }

            // Default list container resolution if not explicitly matched in registry
            if (containerClass == null) {
                String defaultContainerId = (!list.isEmpty() && list.get(0) instanceof Displayable) ? "tabs" : "vbox";
                Class<? extends ParameterRenderer<?>> defaultClass = ID_REGISTRY.get(defaultContainerId);
                if (defaultClass != null && AbstractListParameterRenderer.class.isAssignableFrom(defaultClass)) {
                    containerClass = defaultClass;
                } else {
                    containerClass = VBoxListParameterRenderer.class;
                }
            }

            try {
                AbstractListParameterRenderer<Object> listRenderer = (AbstractListParameterRenderer<Object>) containerClass.getDeclaredConstructor().newInstance();
                listRenderer.setItemRendererId(itemRendererId);
                listRenderer.init(agiPanel, call, paramName, (List<Object>) (List<?>) list);
                return listRenderer;
            } catch (Exception e) {
                log.error("Failed to instantiate list container renderer: {}", containerClass.getName(), e);
            }
        }

        // 2. Single Item: Check String ID hits (avoiding list containers if single item)
        String singleItemId = token0;
        if (token0 != null) {
            Class<? extends ParameterRenderer<?>> clazz0 = ID_REGISTRY.get(token0);
            if (clazz0 != null && AbstractListParameterRenderer.class.isAssignableFrom(clazz0) && token1 != null) {
                singleItemId = token1;
            }
        }

        if (singleItemId != null) {
            Class<? extends ParameterRenderer<?>> rendererClass = ID_REGISTRY.get(singleItemId);
            if (rendererClass != null && !AbstractListParameterRenderer.class.isAssignableFrom(rendererClass)) {
                try {
                    ParameterRenderer<Object> renderer = (ParameterRenderer<Object>) rendererClass.getDeclaredConstructor().newInstance();
                    renderer.init(agiPanel, call, paramName, value);
                    return renderer;
                } catch (Exception e) {
                    log.error("Failed to instantiate ID-based specialized renderer for id: {}", singleItemId, e);
                }
            }
        }

        // 3. Single Item: Check for Specialized Registry Hits by Class
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

        // 4. Authoritative Fallback: High-Fidelity Object-to-String Renderer
        String lang = (singleItemId != null) ? singleItemId : "text";

        ObjectToStringParameterRenderer renderer = new ObjectToStringParameterRenderer();
        renderer.setLanguage(lang);
        renderer.setEditable(value instanceof String); // Policy: Only pure Strings are editable for now
        renderer.init(agiPanel, call, paramName, value);
        
        return renderer;
    }
}
