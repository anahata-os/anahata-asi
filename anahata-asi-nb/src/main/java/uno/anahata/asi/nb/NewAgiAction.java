/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionRegistration;

/**
 * Action that creates and opens a brand-new AGI session in the NetBeans IDE.
 * <p>
 * Invokes {@link NetBeansAsiContainer#createNewAgi()} to instantiate a session
 * with default template preferences and opens its corresponding {@link AgiTopComponent}.
 * </p>
 *
 * @author anahata
 */
@ActionID(
        category = "Window",
        id = "uno.anahata.asi.nb.NewAgiAction"
)
@ActionRegistration(
        displayName = "#CTL_NewAgiAction",
        iconBase = "icons/anahata_16.png"
)
@ActionReference(path = "Menu/Window", position = 100)
public final class NewAgiAction implements ActionListener {

    /**
     * {@inheritDoc}
     * <p>
     * Creates a new AGI session via the container and opens its TopComponent tab.
     * </p>
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        AnahataInstaller.getContainer().createNewAgi();
    }
}
