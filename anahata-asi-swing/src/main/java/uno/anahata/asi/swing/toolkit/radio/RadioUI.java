/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.toolkit.radio;

import javax.swing.JPanel;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.toolkit.render.ToolkitUI;
import uno.anahata.asi.yam.tools.Radio;

/**
 * UI strategy implementation for the {@link Radio} toolkit.
 * <p>
 * Binds the specialized {@link RadioRenderer} panel to the toolkit instance.
 * </p>
 *
 * @author anahata
 */
public class RadioUI implements ToolkitUI<Radio> {

    @Override
    public JPanel createPanel(Radio toolkit, AgiPanel agiPanel) {
        RadioRenderer renderer = new RadioRenderer();
        return renderer.createToolkitPanel(toolkit, agiPanel);
    }
}
