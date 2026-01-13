/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi;

import java.awt.BorderLayout;
import lombok.extern.slf4j.Slf4j;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.windows.TopComponent;
import uno.anahata.asi.swing.chat.SessionsPanel;

/**
 * A TopComponent that displays a list of all active Anahata ASI sessions.
 */
@TopComponent.Description(
        preferredID = "SessionsTopComponent",
        iconBase = "icons/anahata_16.png",
        persistenceType = TopComponent.PERSISTENCE_ONLY_OPENED)
@TopComponent.Registration(mode = "explorer", openAtStartup = true, position = 101)
@ActionID(category = "Window", id = "uno.anahata.asi.SessionsTopComponent")
@ActionReference(path = "Menu/Window", position = 101)
@TopComponent.OpenActionRegistration(
        displayName = "ASI Sessions",
        preferredID = "SessionsTopComponent"
)
@Slf4j
public class SessionsTopComponent extends TopComponent {

    private final SessionsPanel sessionsPanel;

    public SessionsTopComponent() {
        setName("Anahata ASI Sessions");
        setToolTipText("Manage active ASI sessions");
        setLayout(new BorderLayout());

        // Use the shared AsiConfig from the installer
        sessionsPanel = new SessionsPanel(AnahataInstaller.getAsiConfig());
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
}
