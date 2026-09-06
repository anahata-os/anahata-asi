/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.provider;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.AbstractAiProviderPanel;

/**
 * A centralized, thread-safe UI registry for mapping AI provider classes to their specialized Swing configuration panels.
 * <p>
 * This registry maintains clean architectural boundaries by allowing {@code anahata-asi-core} to remain 100% UI-agnostic
 * while enabling {@code anahata-asi-swing} to register and instantiate custom provider panels (such as {@link GeminiAiProviderPanel}
 * or {@link AnthropicProviderPanel}).
 * </p>
 * <p>
 * <b>Inheritance Walk-Up:</b> When looking up a panel class for a concrete provider, this registry automatically walks up the
 * class hierarchy until a registered panel class is found, defaulting cleanly to {@link AbstractAiProviderPanel} if no specialized panel
 * is registered for that provider type.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class AiProviderUiRegistry {

    /**
     * Singleton instance of the provider UI registry.
     */
    private static final AiProviderUiRegistry INSTANCE = new AiProviderUiRegistry();

    /**
     * The backing concurrent map mapping provider domain types to their corresponding Swing panel classes.
     */
    private final Map<Class<? extends AbstractAiProvider>, Class<? extends AbstractAiProviderPanel<?>>> registry = new ConcurrentHashMap<>();

    /**
     * Private constructor to enforce singleton pattern.
     */
    private AiProviderUiRegistry() {
    }

    /**
     * Retrieves the global singleton instance of the AI Provider UI registry.
     *
     * @return The global {@link AiProviderUiRegistry} instance.
     */
    public static AiProviderUiRegistry getInstance() {
        return INSTANCE;
    }

    /**
     * Registers a custom Swing panel class for a specific AI provider type.
     *
     * @param <P> The provider type.
     * @param <U> The panel type.
     * @param providerClass The domain class of the AI provider.
     * @param panelClass The Swing panel class responsible for configuring the provider.
     */
    @SuppressWarnings("unchecked")
    public <P extends AbstractAiProvider, U extends AbstractAiProviderPanel<P>> void register(
            @NonNull Class<P> providerClass,
            @NonNull Class<U> panelClass
    ) {
        registry.put(providerClass, (Class<? extends AbstractAiProviderPanel<?>>) (Class<?>) panelClass);
        log.info("Registered AI provider UI panel: {} -> {}", providerClass.getSimpleName(), panelClass.getSimpleName());
    }

    /**
     * Resolves the most specific {@link AbstractAiProviderPanel} class for a given AI provider type.
     * <p>
     * Walks up the superclass hierarchy starting from {@code providerClass} up to {@link AbstractAiProvider}
     * to find the closest registered panel class. If no specialized mapping is found, returns {@link AbstractAiProviderPanel}.
     * </p>
     *
     * @param providerClass The concrete class of the AI provider.
     * @return The resolved {@link AbstractAiProviderPanel} class.
     */
    @SuppressWarnings("unchecked")
    public Class<? extends AbstractAiProviderPanel<?>> getPanelClass(@NonNull Class<? extends AbstractAiProvider> providerClass) {
        Class<?> curr = providerClass;
        while (curr != null && AbstractAiProvider.class.isAssignableFrom(curr)) {
            Class<? extends AbstractAiProviderPanel<?>> panelClass = registry.get(curr);
            if (panelClass != null) {
                return panelClass;
            }
            curr = curr.getSuperclass();
        }
        return (Class<? extends AbstractAiProviderPanel<?>>) (Class<?>) AbstractAiProviderPanel.class;
    }

    /**
     * Instantiates and initializes the appropriate typed {@link AbstractAiProviderPanel} for the given AI provider.
     *
     * @param <P> The provider type.
     * @param container The parent ASI container instance.
     * @param provider The AI provider entity to configure.
     * @param removeCallback The callback to execute when the user deletes the provider.
     * @return A newly instantiated and initialized {@link AbstractAiProviderPanel} instance.
     */
    @SuppressWarnings("unchecked")
    public <P extends AbstractAiProvider> AbstractAiProviderPanel<P> createPanel(
            @NonNull AbstractSwingAsiContainer container,
            @NonNull P provider,
            Runnable removeCallback
    ) {
        Class<? extends AbstractAiProviderPanel<?>> panelClass = getPanelClass(provider.getClass());
        try {
            AbstractAiProviderPanel<P> panel = (AbstractAiProviderPanel<P>) panelClass.getDeclaredConstructor().newInstance();
            panel.init(container, provider, removeCallback);
            return panel;
        } catch (Exception e) {
            log.error("Failed to instantiate custom provider panel '{}', falling back to base AiProviderPanel", panelClass.getName(), e);
            AbstractAiProviderPanel<P> fallback = new AbstractAiProviderPanel<>();
            fallback.init(container, provider, removeCallback);
            return fallback;
        }
    }
}
