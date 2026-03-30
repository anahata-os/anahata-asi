/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb;

import java.awt.BorderLayout;
import lombok.extern.slf4j.Slf4j;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.windows.TopComponent;
import uno.anahata.asi.swing.AsiTableContainerPanel;

/**
 * A TopComponent that displays a list of all active Anahata ASI sessions in a table.
 * This provides a compact, searchable alternative to the Cards view.
 * 
 * @author anahata
 */
@TopComponent.Description(
        preferredID = "AsiTableTopComponent",
        iconBase = "icons/anahata_16.png",
        persistenceType = TopComponent.PERSISTENCE_ONLY_OPENED)
@TopComponent.Registration(mode = "output", openAtStartup = false, position = 109)
@ActionID(category = "Window", id = "uno.anahata.asi.OpenAsiTableTopComponent")
@ActionReference(path = "Menu/Window", position = 102)
@TopComponent.OpenActionRegistration(
        displayName = "ASI Container (Table)",
        preferredID = "asiTable"
)
@Slf4j
public class AsiTableTopComponent extends TopComponent {

    /** The UI panel displaying the active sessions in a table. */
    private final AsiTableContainerPanel sessionsPanel;

    /**
     * Default constructor for the table view.
     */
    public AsiTableTopComponent() {
        setName("Anahata ASI (Table)");
        setToolTipText("Manage active AGI sessions in a tabular view");
        setLayout(new BorderLayout());

        // Use the shared AsiContainer from the installer
        NetBeansAsiContainer container = AnahataInstaller.getContainer();
        sessionsPanel = new AsiTableContainerPanel(container);
        add(sessionsPanel, BorderLayout.CENTER);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Starts the periodic refresh of the sessions table.
     * </p>
     */
    @Override
    public void componentOpened() {
        sessionsPanel.startRefresh();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Stops the periodic refresh of the sessions table.
     * </p>
     */
    @Override
    public void componentClosed() {
        sessionsPanel.stopRefresh();
    }
}
