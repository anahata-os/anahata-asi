/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.beans.PropertyChangeEvent;
import java.io.Serializable;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.SwingWorker;
import lombok.extern.slf4j.Slf4j;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.windows.TopComponent;
import org.openide.util.ImageUtilities;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.status.AgiStatus;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.SwingAgiConfig;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * The main TopComponent for Anahata ASI V2.
 * It manages a single agi session and its corresponding UI panel.
 * <p>
 * This component uses the {@code writeReplace} pattern for robust persistence,
 * ensuring that only the session ID is saved and the component is correctly
 * reconstructed after an IDE restart or a module reload.
 * </p>
 * 
 * @author anahata
 */
@ActionID(category = "Window", id = "uno.anahata.asi.OpenAgiTopComponent")
@ActionReference(path = "Menu/Window", position = 334)
@TopComponent.Description(
        preferredID = "agi",
        iconBase = "icons/anahata_16.png",
        persistenceType = TopComponent.PERSISTENCE_ONLY_OPENED)
@TopComponent.Registration(mode = "output", openAtStartup = false, position = 0)
@TopComponent.OpenActionRegistration(displayName = "AGI Container")
@Slf4j
public final class AgiTopComponent extends TopComponent {

    /** The UI panel for the agi session. */
    private transient AgiPanel agiPanel;
    
    /** The unique ID of the session managed by this component. */
    private String sessionId;

    /**
     * Default constructor required for NetBeans persistence mechanism.
     * It creates a component without a session ID, which will be populated
     * during restoration or initialization.
     */
    public AgiTopComponent() {
        this((String)null);
    }
    
    /**
     * Constructs a new component for an existing agi session.
     * 
     * @param agi The agi session.
     */
    public AgiTopComponent(Agi agi) {
        this(agi.getConfig().getSessionId());
        initPanel(agi);
    }

    /**
     * Constructs a new component for a specific session ID.
     * 
     * @param sessionId The session ID.
     */
    public AgiTopComponent(String sessionId) {
        this.sessionId = sessionId;
        setIcon(ImageUtilities.loadImage("icons/anahata_16.png"));
        setLayout(new BorderLayout());
        updateTitles();
    }

    /**
     * Initializes the agi panel and adds it to the component.
     * 
     * @param agi The agi session.
     */
    private void initPanel(Agi agi) {
        if (agiPanel != null) return;
        this.sessionId = agi.getConfig().getSessionId();
        agiPanel = new AgiPanel(agi);
        agiPanel.initComponents();
        
        removeAll();
        add(agiPanel, BorderLayout.CENTER);
        revalidate();
        repaint();
        
        updateTitles();
        
        // Listen for nickname changes to update the TopComponent title
        new EdtPropertyChangeListener(this, agi, "nickname", this::handleNicknameChange);
        
        // Listen for status changes to update the tab color
        new EdtPropertyChangeListener(this, agi.getStatusManager(), "currentStatus", evt -> updateTitles());
    }

    /**
     * Updates the TopComponent's name, display name, and tooltip based on the current agi session.
     */
    private void updateTitles() {
        String displayName = sessionId != null ? "Session: " + sessionId.substring(0, 7) : "Anahata AGI";
        AgiStatus status = AgiStatus.IDLE;
        
        Agi agi = getAgi();
        if (agi == null && sessionId != null) {
            // Try to find the agi in the container even if the panel isn't ready
            agi = AnahataInstaller.getContainer().getActiveAgis().stream()
                    .filter(c -> c.getConfig().getSessionId().equals(sessionId))
                    .findFirst().orElse(null);
        }
        
        if (agi != null) {
            displayName = agi.getDisplayName();
            status = agi.getStatusManager().getCurrentStatus();
        }
        
        Color color = SwingAgiConfig.getColor(status);
        String hexColor = SwingUtils.toHtmlColor(color);
        
        setName(displayName);
        setDisplayName(displayName);
        setHtmlDisplayName("<html><font color='" + hexColor + "'>" + displayName + "</font></html>");
        
        String tooltip = sessionId != null ? "Anahata Session: " + sessionId + " [" + status.getDisplayName() + "]" : "Anahata AGI";
        setToolTipText(tooltip);
    }

    /**
     * Handles nickname changes by updating the TopComponent titles.
     * 
     * @param evt The property change event.
     */
    private void handleNicknameChange(PropertyChangeEvent evt) {
        updateTitles();
    }

    /**
     * {@inheritDoc}
     * Ensures the agi panel is initialized when the component is opened.
     * Uses a SwingWorker to avoid blocking the EDT during heavy initialization.
     */
    @Override
    public void componentOpened() {
        if (agiPanel == null) {
            showLoading();
            
            new SwingWorker<Agi, Void>() {
                @Override
                protected Agi doInBackground() throws Exception {
                    log.info("Initializing session brain in background: {}", sessionId);
                    NetBeansAsiContainer container = (NetBeansAsiContainer) AnahataInstaller.getContainer();
                    return container.findOrCreateAgi(sessionId);
                }

                @Override
                protected void done() {
                    try {
                        Agi agi = get();
                        initPanel(agi);
                        log.info("Session brain initialized OK: {}", agi.getShortId());
                    } catch (Exception e) {
                        log.error("Failed to initialize session brain", e);
                        showError(e.getMessage());
                    }
                }
            }.execute();
        }
    }

    private void showLoading() {
        removeAll();
        JPanel loadingPanel = new JPanel(new BorderLayout());
        JLabel label = new JLabel("Initializing Blaugrana Brain...", SwingConstants.CENTER);
        label.setFont(label.getFont().deriveFont(Font.BOLD, 14f));
        loadingPanel.add(label, BorderLayout.CENTER);
        add(loadingPanel, BorderLayout.CENTER);
        revalidate();
        repaint();
    }

    private void showError(String message) {
        removeAll();
        JPanel errorPanel = new JPanel(new BorderLayout());
        JLabel label = new JLabel("<html><center>Failed to load session:<br><font color='red'>" + message + "</font></center></html>", SwingConstants.CENTER);
        errorPanel.add(label, BorderLayout.CENTER);
        add(errorPanel, BorderLayout.CENTER);
        revalidate();
        repaint();
    }

    /**
     * Gets the agi session managed by this component.
     * 
     * @return The agi session, or null if not initialized.
     */
    public Agi getAgi() {
        return agiPanel != null ? agiPanel.getAgi() : null;
    }

    /**
     * Returns a stable, session-specific ID for the window system.
     * 
     * @return The preferred ID.
     */
    @Override
    protected String preferredID() {
        return "agi_" + (sessionId != null ? sessionId : "new");
    }

    /**
     * The standard NetBeans persistence pattern: replace the component with a 
     * serializable proxy that only holds the essential state (the session ID).
     * 
     * @return A serializable Resolvable object.
     */
    @Override
    protected Object writeReplace() throws java.io.ObjectStreamException {
        return new Resolvable(sessionId);
    }

    /**
     * A static inner class used for serializing the state of an AgiTopComponent.
     */
    private static final class Resolvable implements Serializable {
        private static final long serialVersionUID = 1L;
        private final String sessionId;

        Resolvable(String sessionId) {
            this.sessionId = sessionId;
        }

        /**
         * Reconstructs the AgiTopComponent using the saved session ID.
         * 
         * @return A new AgiTopComponent instance.
         */
        Object readResolve() throws java.io.ObjectStreamException {
            return new AgiTopComponent(sessionId);
        }
    }
}
