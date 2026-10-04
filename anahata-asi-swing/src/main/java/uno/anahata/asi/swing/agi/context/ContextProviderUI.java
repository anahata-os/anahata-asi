/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.context;

import javax.swing.JPanel;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * Universal UI strategy interface for a specific context provider type.
 * <p>
 * Encapsulates the visual presentation of a context provider across both the Context explorer
 * tree node (left pane) and its specialized detail panel (right pane), providing a clean,
 * decoupled, and non-invasive extension point for provider authors.
 * </p>
 *
 * @param <T> The strongly-typed context provider class.
 * @author anahata
 */
public interface ContextProviderUI<T extends ContextProvider> {

    /**
     * Creates a specialized context tree node for this provider,
     * or returns {@code null} to use the default {@link ContextProviderNode}.
     *
     * @param agiPanel The active parent AGI panel.
     * @param provider The strongly-typed context provider instance.
     * @return A specialized context node, or null for default.
     */
    default AbstractContextNode<?> createNode(AgiPanel agiPanel, T provider) {
        return null;
    }

    /**
     * Creates the specialized detail panel for this provider,
     * or returns {@code null} if this provider has no custom panel.
     *
     * @param provider The strongly-typed context provider instance.
     * @param contextPanel The active parent ContextPanel.
     * @return A custom JPanel, or null if no custom panel.
     */
    default JPanel createPanel(T provider, ContextPanel contextPanel) {
        return null;
    }
}
