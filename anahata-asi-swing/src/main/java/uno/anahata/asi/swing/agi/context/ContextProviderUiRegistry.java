/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.context;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import javax.swing.JPanel;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.context.ContextProvider;

/**
 * A singleton registry for mapping context provider classes to their specialized UI renderer classes.
 * <p>
 * This registry acts as a factory, creating new renderer instances for each context provider
 * instance to ensure that UIs are not shared across different AGI sessions.
 * It supports hierarchical lookup and automatic binding.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ContextProviderUiRegistry {

    private static final ContextProviderUiRegistry INSTANCE = new ContextProviderUiRegistry();

    /**
     * Gets the singleton registry instance.
     *
     * @return The singleton ContextProviderUiRegistry instance.
     */
    public static ContextProviderUiRegistry getInstance() {
        return INSTANCE;
    }

    private final Map<Class<? extends ContextProvider>, Class<? extends AbstractContextProviderRenderer<?>>> rendererClasses = new HashMap<>();

    /**
     * Registers a specialized renderer class for a context provider type.
     *
     * @param <T> The context provider type.
     * @param providerClass The context provider class.
     * @param rendererClass The specialized renderer class extending AbstractContextProviderRenderer.
     */
    public <T extends ContextProvider> void register(Class<T> providerClass, Class<? extends AbstractContextProviderRenderer<T>> rendererClass) {
        rendererClasses.put(providerClass, rendererClass);
    }

    /**
     * Creates and binds a new renderer instance for the given context provider.
     *
     * @param provider The context provider instance.
     * @param parent The parent ContextPanel.
     * @return An Optional containing the bound JPanel if a renderer was registered.
     */
    @SuppressWarnings("unchecked")
    public Optional<JPanel> createRenderer(ContextProvider provider, ContextPanel parent) {
        return findRendererClass(provider.getClass())
                .flatMap(rendererClass -> {
                    try {
                        AbstractContextProviderRenderer<ContextProvider> renderer =
                                (AbstractContextProviderRenderer<ContextProvider>) rendererClass.getDeclaredConstructor().newInstance();
                        return Optional.of(renderer.createProviderPanel(provider, parent));
                    } catch (Exception e) {
                        log.error("Failed to instantiate context provider renderer: " + rendererClass.getName(), e);
                        return Optional.empty();
                    }
                });
    }

    @SuppressWarnings("unchecked")
    private <T extends ContextProvider> Optional<Class<? extends AbstractContextProviderRenderer<T>>> findRendererClass(Class<?> providerClass) {
        Class<?> current = providerClass;
        while (current != null && ContextProvider.class.isAssignableFrom(current)) {
            Class<? extends AbstractContextProviderRenderer<?>> rendererClass = rendererClasses.get(current);
            if (rendererClass != null) {
                return Optional.of((Class<? extends AbstractContextProviderRenderer<T>>) rendererClass);
            }
            current = current.getSuperclass();
        }
        return Optional.empty();
    }
}
