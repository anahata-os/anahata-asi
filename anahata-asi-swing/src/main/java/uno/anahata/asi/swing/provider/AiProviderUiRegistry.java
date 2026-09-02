/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.provider;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.AiProviderPanel;

/**
 * A centralized, thread-safe UI registry for mapping AI provider classes to their specialized Swing configuration panels.
 * <p>
 * This registry maintains clean architectural boundaries by allowing {@code anahata-asi-core} to remain 100% UI-agnostic
 * while enabling {@code anahata-asi-swing} to register and instantiate custom provider panels (such as {@link GeminiAiProviderPanel}
 * or {@link AnthropicProviderPanel}).
 * </p>
 * <p>
 * <b>Inheritance Walk-Up:</b> When looking up a panel class for a concrete provider, this registry automatically walks up the
 * class hierarchy until a registered panel class is found, defaulting cleanly to {@link AiProviderPanel} if no specialized panel
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
    private final Map<Class<? extends AbstractAiProvider>, Class<? extends AiProviderPanel>> registry = new ConcurrentHashMap<>();

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
    public <P extends AbstractAiProvider, U extends AiProviderPanel> void register(
            @NonNull Class<P> providerClass,
            @NonNull Class<U> panelClass
    ) {
        registry.put(providerClass, panelClass);
        log.info("Registered AI provider UI panel: {} -> {}", providerClass.getSimpleName(), panelClass.getSimpleName());
    }

    /**
     * Resolves the most specific {@link AiProviderPanel} class for a given AI provider type.
     * <p>
     * Walks up the superclass hierarchy starting from {@code providerClass} up to {@link AbstractAiProvider}
     * to find the closest registered panel class. If no specialized mapping is found, returns {@link AiProviderPanel#class}.
     * </p>
     *
     * @param providerClass The concrete class of the AI provider.
     * @return The resolved {@link AiProviderPanel} class.
     */
    public Class<? extends AiProviderPanel> getPanelClass(@NonNull Class<? extends AbstractAiProvider> providerClass) {
        Class<?> curr = providerClass;
        while (curr != null && AbstractAiProvider.class.isAssignableFrom(curr)) {
            Class<? extends AiProviderPanel> panelClass = registry.get(curr);
            if (panelClass != null) {
                return panelClass;
            }
            curr = curr.getSuperclass();
        }
        return AiProviderPanel.class;
    }

    /**
     * Instantiates and initializes the appropriate typed {@link AiProviderPanel} for the given AI provider.
     *
     * @param container The parent ASI container instance.
     * @param provider The AI provider entity to configure.
     * @param removeCallback The callback to execute when the user deletes the provider.
     * @return A newly instantiated and initialized {@link AiProviderPanel} instance.
     */
    public AiProviderPanel createPanel(
            @NonNull AbstractSwingAsiContainer container,
            @NonNull AbstractAiProvider provider,
            Runnable removeCallback
    ) {
        Class<? extends AiProviderPanel> panelClass = getPanelClass(provider.getClass());
        try {
            AiProviderPanel panel = panelClass.getDeclaredConstructor().newInstance();
            panel.init(container, provider, removeCallback);
            return panel;
        } catch (Exception e) {
            log.error("Failed to instantiate custom provider panel '{}', falling back to base AiProviderPanel", panelClass.getName(), e);
            AiProviderPanel fallback = new AiProviderPanel();
            fallback.init(container, provider, removeCallback);
            return fallback;
        }
    }
}
