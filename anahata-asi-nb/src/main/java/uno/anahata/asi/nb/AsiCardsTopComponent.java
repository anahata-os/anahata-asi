/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb;

import java.awt.BorderLayout;
import java.util.Set;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager; 
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.swing.AsiCardsContainerPanel;
import uno.anahata.asi.swing.AgiController;

/**
 * A TopComponent that displays a list of all active Anahata ASI sessions.
 * It uses the switcher view which defaults to Sticky Notes (Cards).
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
public class AsiCardsTopComponent extends TopComponent implements AgiController {

    private final AsiCardsContainerPanel sessionsPanel;

    public AsiCardsTopComponent() {
        setName("Anahata ASI");
        setToolTipText("Manage active AGI sessions");
        setLayout(new BorderLayout());

        // Use the shared AsiContainer from the installer
        sessionsPanel = new AsiCardsContainerPanel(AnahataInstaller.getContainer());
        sessionsPanel.setController(this);
        add(sessionsPanel, BorderLayout.CENTER);
    }

    @Override
    public void componentOpened() {
        sessionsPanel.startRefresh();
    }

    @Override
    public void componentClosed() {
        sessionsPanel.stopRefresh();
    }

    @Override
    public void focus(@NonNull Agi agi) {
        Set<TopComponent> opened = WindowManager.getDefault().getRegistry().getOpened();
        for (TopComponent tc : opened) {
            if (tc instanceof AgiTopComponent atc) {
                if (atc.getAgi() == agi) {
                    atc.open();
                    atc.requestActive();
                    return;
                }
            }
        }
        
        // If not found, open a new one for this session
        AgiTopComponent tc = new AgiTopComponent(agi);
        tc.open();
        tc.requestActive();
    }

    @Override
    public void close(@NonNull Agi agi) {
        Set<TopComponent> opened = WindowManager.getDefault().getRegistry().getOpened();
        for (TopComponent tc : opened) {
            if (tc instanceof AgiTopComponent atc) {
                if (atc.getAgi() == agi) {
                    atc.close();
                    return;
                }
            }
        }
    }

}
