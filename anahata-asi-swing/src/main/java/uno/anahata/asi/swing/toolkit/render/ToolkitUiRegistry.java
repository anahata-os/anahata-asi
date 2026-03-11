/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.toolkit.render;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import javax.swing.JPanel;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.spi.AbstractToolkit;
import uno.anahata.asi.agi.tool.spi.java.JavaObjectToolkit;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.yam.tools.Radio;

/**
 * A singleton registry for mapping toolkit classes to their specialized UI renderer classes.
 * <p>
 * This registry acts as a factory, creating new renderer instances for each toolkit
 * instance to ensure that UIs are not shared across different AGI sessions.
 * It supports hierarchical lookup and automatic binding.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ToolkitUiRegistry {

    private static final ToolkitUiRegistry INSTANCE = new ToolkitUiRegistry();

    /**
     * Gets the singleton registry instance.
     * @return The registry.
     */
    public static ToolkitUiRegistry getInstance() {
        return INSTANCE;
    }

    static {
        // Register default toolkit renderers
        INSTANCE.register(Radio.class, RadioRenderer.class);
    }

    /** Map of toolkit class to renderer class. */
    private final Map<Class<? extends AnahataToolkit>, Class<? extends ToolkitRenderer<?>>> rendererClasses = new HashMap<>();

    /**
     * Registers a renderer class for a specific toolkit class.
     * 
     * @param <T> The toolkit type.
     * @param toolkitClass The toolkit class.
     * @param rendererClass The renderer class.
     */
    public <T extends AnahataToolkit> void register(Class<T> toolkitClass, Class<? extends ToolkitRenderer<T>> rendererClass) {
        rendererClasses.put(toolkitClass, rendererClass);
    }

    /**
     * Creates and binds a new renderer instance for the given toolkit wrapper.
     * <p>
     * This method extracts the underlying {@link AnahataToolkit} instance, 
     * performs a hierarchical lookup for a renderer class, and instantiates it.
     * </p>
     * 
     * @param toolkit The abstract toolkit wrapper.
     * @param parent The parent AgiPanel.
     * @return An Optional containing the bound JPanel if a renderer was created.
     */
    @SuppressWarnings("unchecked")
    public Optional<JPanel> createRenderer(AbstractToolkit<?> toolkit, AgiPanel parent) {
        if (toolkit instanceof JavaObjectToolkit jot && jot.getToolkitInstance() instanceof AnahataToolkit atk) {
            return findRendererClass((Class<AnahataToolkit>) atk.getClass())
                    .flatMap(rendererClass -> {
                        try {
                            ToolkitRenderer<AnahataToolkit> renderer = (ToolkitRenderer<AnahataToolkit>) rendererClass.getDeclaredConstructor().newInstance();
                            return Optional.of(renderer.bind(atk, parent));
                        } catch (Exception e) {
                            log.error("Failed to instantiate toolkit renderer: " + rendererClass.getName(), e);
                            return Optional.empty();
                        }
                    });
        }
        return Optional.empty();
    }

    /**
     * Finds a renderer class for the given toolkit class by traversing up the hierarchy.
     * 
     * @param <T> The toolkit type.
     * @param toolkitClass The toolkit class.
     * @return An Optional containing the renderer class if found.
     */
    @SuppressWarnings("unchecked")
    private <T extends AnahataToolkit> Optional<Class<? extends ToolkitRenderer<T>>> findRendererClass(Class<T> toolkitClass) {
        Class<?> current = toolkitClass;
        while (current != null && AnahataToolkit.class.isAssignableFrom(current)) {
            Class<? extends ToolkitRenderer<?>> rendererClass = rendererClasses.get(current);
            if (rendererClass != null) {
                return Optional.of((Class<? extends ToolkitRenderer<T>>) rendererClass);
            }
            current = current.getSuperclass();
        }
        return Optional.empty();
    }
}
