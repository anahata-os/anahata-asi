/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb;

import java.awt.BorderLayout;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.windows.TopComponent;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.AsiCardsContainerPanel;

/**
 * A TopComponent that displays a list of all active Anahata ASI sessions.
 * It uses the switcher view which defaults to Sticky Notes (Cards).
 * 
 * @author anahata
 */
@TopComponent.Description(
        preferredID = "AsiCardsTopComponent",
        iconBase = "icons/anahata_16.png",
        persistenceType = TopComponent.PERSISTENCE_ONLY_OPENED)
@TopComponent.Registration(mode = "navigator", openAtStartup = true, position = 108)
@ActionID(category = "Window", id = "uno.anahata.asi.OpenAsiCardsTopComponent")
@ActionReference(path = "Menu/Window", position = 101)
@TopComponent.OpenActionRegistration(
        displayName = "ASI Container (Cards)",
        preferredID = "asi"
)
@Slf4j
public class AsiCardsTopComponent extends TopComponent {

    /** The UI panel displaying the active sessions as cards. */
    private final AsiCardsContainerPanel sessionsPanel;

    /**
     * Default constructor for the cards view.
     */
    public AsiCardsTopComponent() {
        setName("Anahata ASI");
        setToolTipText("Manage active AGI sessions");
        setLayout(new BorderLayout());

        // Use the shared AsiContainer from the installer, casting to the Swing-aware base
        AbstractSwingAsiContainer container = (AbstractSwingAsiContainer) AnahataInstaller.getContainer();
        sessionsPanel = new AsiCardsContainerPanel(container);
        add(sessionsPanel, BorderLayout.CENTER);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Starts the periodic refresh of the sessions panel.
     * </p>
     */
    @Override
    public void componentOpened() {
        sessionsPanel.startRefresh();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Stops the periodic refresh of the sessions panel.
     * </p>
     */
    @Override
    public void componentClosed() {
        sessionsPanel.stopRefresh();
    }
}
