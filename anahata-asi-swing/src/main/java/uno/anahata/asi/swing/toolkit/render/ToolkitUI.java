/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.toolkit.render;

import javax.swing.JPanel;
import uno.anahata.asi.agi.tool.spi.java.JavaObjectToolkit;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.context.AbstractContextNode;
import uno.anahata.asi.swing.agi.context.ToolkitNode;

/**
 * Universal UI strategy interface for a specific toolkit type.
 * <p>
 * Encapsulates the visual presentation of a toolkit across both the Context explorer
 * tree node (left pane) and its specialized detail panel (right pane), providing a clean,
 * decoupled, and non-invasive extension point for toolkit authors.
 * </p>
 *
 * @param <T> The strongly-typed toolkit instance class.
 * @author anahata
 */
public interface ToolkitUI<T> {

    /**
     * Creates a specialized context tree node for this toolkit,
     * or returns {@code null} to use the default {@link ToolkitNode}.
     *
     * @param agiPanel The active parent AGI panel.
     * @param toolkit The strongly-typed toolkit instance.
     * @param toolkitWrapper The JavaObjectToolkit wrapper.
     * @return A specialized context node, or null for default.
     */
    default AbstractContextNode<?> createNode(AgiPanel agiPanel, T toolkit, JavaObjectToolkit toolkitWrapper) {
        return null;
    }

    /**
     * Creates the specialized detail panel for this toolkit,
     * or returns {@code null} if this toolkit has no custom panel.
     *
     * @param toolkit The strongly-typed toolkit instance.
     * @param agiPanel The active parent AGI panel.
     * @return A custom JPanel, or null if no custom panel.
     */
    default JPanel createPanel(T toolkit, AgiPanel agiPanel) {
        return null;
    }
}
