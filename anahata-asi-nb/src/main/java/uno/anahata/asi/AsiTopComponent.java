/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi;

import java.awt.BorderLayout;
import java.util.Set;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.swing.chat.AsiSwitcherContainerPanel;
import uno.anahata.asi.swing.chat.SessionController;

/**
 * A TopComponent that displays a list of all active Anahata ASI sessions.
 * It uses the switcher view which defaults to Sticky Notes (Cards).
 */
@TopComponent.Description(
        preferredID = "AsiTopComponent",
        iconBase = "icons/anahata_16.png",
        persistenceType = TopComponent.PERSISTENCE_ONLY_OPENED)
@TopComponent.Registration(mode = "navigator", openAtStartup = true, position = 108)
@ActionID(category = "Window", id = "uno.anahata.asi.OpenAsiTopComponent")
@ActionReference(path = "Menu/Window", position = 101)
@TopComponent.OpenActionRegistration(
        displayName = "ASI Sessions",
        preferredID = "SessionsTopComponent"
)
@Slf4j
public class AsiTopComponent extends TopComponent implements SessionController {

    private final AsiSwitcherContainerPanel sessionsPanel;

    public AsiTopComponent() {
        setName("ASI");
        setToolTipText("Manage active AGI sessions");
        setLayout(new BorderLayout());

        // Use the shared AsiContainer from the installer
        sessionsPanel = new AsiSwitcherContainerPanel(AnahataInstaller.getContainer());
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
    public void focus(@NonNull Chat chat) {
        Set<TopComponent> opened = WindowManager.getDefault().getRegistry().getOpened();
        for (TopComponent tc : opened) {
            if (tc instanceof AgiTopComponent atc) {
                if (atc.getChat() == chat) {
                    atc.open();
                    atc.requestActive();
                    return;
                }
            }
        }
        
        // If not found, open a new one for this session
        AgiTopComponent tc = new AgiTopComponent();
        tc.setSessionIdForHandoff(chat.getConfig().getSessionId());
        tc.open();
        tc.requestActive();
    }

    @Override
    public void close(@NonNull Chat chat) {
        Set<TopComponent> opened = WindowManager.getDefault().getRegistry().getOpened();
        for (TopComponent tc : opened) {
            if (tc instanceof AgiTopComponent atc) {
                if (atc.getChat() == chat) {
                    atc.close();
                    return;
                }
            }
        }
    }

    @Override
    public void dispose(@NonNull Chat chat) {
        close(chat);
        AnahataInstaller.getContainer().dispose(chat);
    }

    @Override
    public void createNew() {
        AgiTopComponent tc = new AgiTopComponent();
        tc.open();
        tc.requestActive();
    }
}
