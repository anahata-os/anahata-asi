/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.uc;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionRegistration;

/**
 * NetBeans Window menu action that opens the Anahata ASI Update Center and
 * Plugin Installer dialog.
 * <p>
 * Registered under {@code Menu/Window} at position 105, directly accessible
 * from the main NetBeans menu bar.
 * </p>
 *
 * @author anahata
 */
@ActionID(
        category = "Window",
        id = "uno.anahata.asi.nb.uc.OpenUpdateCenterAction"
)
@ActionRegistration(
        displayName = "#CTL_OpenUpdateCenterAction",
        iconBase = "icons/anahata_16.png"
)
@ActionReference(path = "Menu/Window", position = 105)
public final class OpenUpdateCenterAction implements ActionListener {

    /**
     * {@inheritDoc}
     * <p>
     * Displays the Anahata ASI Update Center dialog on the Event Dispatch Thread.
     * </p>
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        AnahataUpdateCenterDialog.showDialog();
    }
}
