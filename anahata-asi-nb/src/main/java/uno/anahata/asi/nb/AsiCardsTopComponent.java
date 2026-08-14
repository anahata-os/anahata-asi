/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb;

import lombok.extern.slf4j.Slf4j;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.windows.TopComponent;
import uno.anahata.asi.swing.AsiCardsContainerPanel;

/**
 * A TopComponent that displays a list of all active Anahata ASI sessions as visual sticky note cards.
 *
 * @author anahata
 */
@TopComponent.Description(
        preferredID = "AsiCardsTopComponent",
        iconBase = "icons/anahata_16.png",
        persistenceType = TopComponent.PERSISTENCE_ALWAYS)
@TopComponent.Registration(mode = "editor", openAtStartup = true, position = 108)
@ActionID(category = "Window", id = "uno.anahata.asi.OpenAsiCardsTopComponent")
@ActionReference(path = "Menu/Window", position = 101)
@TopComponent.OpenActionRegistration(
        displayName = "ASI Container (Cards)",
        preferredID = "AsiCardsTopComponent"
)
@Slf4j
public class AsiCardsTopComponent extends AbstractAsiContainerTopComponent<AsiCardsContainerPanel> {

    /**
     * Default constructor for the cards view.
     */
    public AsiCardsTopComponent() {
        super(new AsiCardsContainerPanel(AnahataInstaller.getContainer()));
        setName("ASI Container (Cards)");
        setToolTipText("Manage active AGI sessions");
    }
}

