/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb;

import lombok.extern.slf4j.Slf4j;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.windows.TopComponent;
import uno.anahata.asi.swing.AsiTableContainerPanel;

/**
 * A TopComponent that displays a list of all active Anahata ASI sessions in a
 * table. This provides a compact, searchable alternative to the Cards view.
 *
 * @author anahata
 */
@TopComponent.Description(
        preferredID = "AsiTableTopComponent",
        iconBase = "icons/anahata_16.png",
        persistenceType = TopComponent.PERSISTENCE_ALWAYS)
@TopComponent.Registration(mode = "output", openAtStartup = false, position = 109)
@ActionID(category = "Window", id = "uno.anahata.asi.OpenAsiTableTopComponent")
@ActionReference(path = "Menu/Window", position = 102)
@TopComponent.OpenActionRegistration(
        displayName = "ASI Container (Table)",
        preferredID = "AsiTableTopComponent"
)
@Slf4j
public class AsiTableTopComponent extends AbstractAsiContainerTopComponent<AsiTableContainerPanel> {

    /**
     * Default constructor for the table view.
     */
    public AsiTableTopComponent() {
        super(new AsiTableContainerPanel(AnahataInstaller.getContainer()));
        setName("ASI Container (Table)");
        setToolTipText("Manage active AGI sessions in a tabular view");
    }
}

