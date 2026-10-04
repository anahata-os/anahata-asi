/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.context;

import java.util.Optional;
import javax.swing.JPanel;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.internal.ClassHierarchyMap;

/**
 * A singleton registry for mapping context provider classes to their specialized {@link ContextProviderUI} strategies.
 * <p>
 * Manages the resolution of both specialized tree nodes and detail panels for context providers,
 * supporting hierarchical lookup using {@link ClassHierarchyMap}.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ContextProviderUiRegistry {

    private static final ContextProviderUiRegistry INSTANCE = new ContextProviderUiRegistry();

    public static ContextProviderUiRegistry getInstance() {
        return INSTANCE;
    }

    private final ClassHierarchyMap<ContextProvider, ContextProviderUI<ContextProvider>> uis = new ClassHierarchyMap<>(ContextProvider.class);

    @SuppressWarnings("unchecked")
    public <T extends ContextProvider> void register(Class<T> providerClass, ContextProviderUI<T> ui) {
        uis.put(providerClass, (ContextProviderUI<ContextProvider>) ui);
    }

    @SuppressWarnings("unchecked")
    public AbstractContextNode<?> createNode(AgiPanel agiPanel, ContextProvider provider) {
        if (provider != null) {
            Optional<ContextProviderUI<ContextProvider>> uiOpt = uis.find(provider.getClass());
            if (uiOpt.isPresent()) {
                return uiOpt.get().createNode(agiPanel, provider);
            }
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    public Optional<JPanel> createRenderer(ContextProvider provider, ContextPanel parent) {
        if (provider != null) {
            Optional<ContextProviderUI<ContextProvider>> uiOpt = uis.find(provider.getClass());
            if (uiOpt.isPresent()) {
                JPanel panel = uiOpt.get().createPanel(provider, parent);
                return Optional.ofNullable(panel);
            }
        }
        return Optional.empty();
    }
}
