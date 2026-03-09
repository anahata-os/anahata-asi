/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.toolkit;

import java.awt.Component;
import javax.swing.JComponent;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.agi.tool.AnahataToolkit;

/**
 * Functional interface for rendering a specialized UI for an {@link AnahataToolkit}.
 * 
 * @param <T> The specific toolkit type.
 * @author anahata
 */
public interface ToolkitRenderer<T extends AnahataToolkit> {

    /**
     * Renders a custom dashboard component for the given toolkit instance.
     * 
     * @param toolkit The toolkit instance.
     * @param parent The parent AgiPanel.
     * @return A JComponent representing the toolkit's UI.
     */
    JComponent render(T toolkit, AgiPanel parent);
}
