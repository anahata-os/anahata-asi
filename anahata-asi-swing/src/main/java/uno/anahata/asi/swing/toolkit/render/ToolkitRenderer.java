/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.toolkit.render;

import javax.swing.JPanel;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.agi.tool.AnahataToolkit;

/**
 * The fundamental contract for any component capable of rendering an
 * {@link AnahataToolkit} in a Swing environment.
 * <p>
 * Implementations of this interface serve as the visual bridge between the
 * toolkit's domain logic and the {@link uno.anahata.asi.swing.agi.AgiPanel}.
 * Renderers must be prepared to be re-bound to different toolkit instances
 * during the session lifecycle.
 * </p>
 *
 * @param <T> The specific toolkit type.
 * @author anahata
 */
public interface ToolkitRenderer<T extends AnahataToolkit> {

    /**
     * Binds the renderer to a specific toolkit instance and its parent panel.
     * <p>
     * This method acts as the "re-createToolkitPanel" point for the UI,
     * ensuring that the component reflects the active toolkit state.
     * </p>
     *
     * @param toolkit The toolkit instance.
     * @param parent The parent AgiPanel.
     * @return A JPanel representing the toolkit's UI (usually 'this').
     */
    JPanel createToolkitPanel(T toolkit, AgiPanel parent);
}
