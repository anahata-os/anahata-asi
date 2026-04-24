/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.beans.PropertyChangeEvent;
import java.io.Serializable;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.windows.TopComponent;
import org.openide.util.ImageUtilities;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.status.AgiStatus;
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
@TopComponent.Description(
        preferredID = "agi",
        iconBase = "icons/anahata_16.png",
        persistenceType = TopComponent.PERSISTENCE_ONLY_OPENED)
@TopComponent.Registration(mode = "editor", openAtStartup = false, position = 102)
@Slf4j
public final class AgiTopComponent extends TopComponent {

    /** The UI panel for the agi session. */
    @Getter
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
     * This method is called on the EDT after the session brain has been 
     * initialized in the background.
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
     * Uses the professional Container executor to avoid blocking the EDT.
     */
    @Override
    public void componentOpened() {
        if (agiPanel == null) {
            showLoading();
            
            // Professional "Birthing Room" Load using the Container's Executor
            AnahataInstaller.getContainer().getExecutor().submit(() -> {
                try {
                    log.info("Initializing session brain in background: {}", sessionId);
                    final Agi agi = AnahataInstaller.getContainer().findOrCreateAgi(sessionId);
                    SwingUtilities.invokeLater(() -> {
                        initPanel(agi);
                        log.info("Session brain initialized OK: {}", agi.getShortId());
                    });
                } catch (Exception e) {
                    log.error("Failed to initialize session brain", e);
                    SwingUtilities.invokeLater(() -> showError(e.getMessage()));
                }
            });
        } else {
            Agi agi = getAgi();
            if (agi != null && !agi.isOpen()) {
                // Authoritative visibility sync
                AnahataInstaller.getContainer().open(agi);
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Authoritatively updates the agi's 'open' status when the component is closed.
     * </p>
     */
    @Override
    protected void componentClosed() {
        Agi agi = getAgi();
        if (agi != null && agi.isOpen()) {
            log.info("Closing TopComponent for agi session: {}. Syncing with container.", agi.getShortId());
            AnahataInstaller.getContainer().close(agi);
        }
    }

    /**
     * Displays a professional loading screen with the Anahata logo and progress bar.
     */
    private void showLoading() {
        removeAll();
        JPanel loadingPanel = new JPanel(new MigLayout("fill, insets 20", "[center, grow]", "[center, grow]"));
        loadingPanel.setBackground(Color.WHITE);
        
        JPanel content = new JPanel(new MigLayout("ins 0, wrap 1", "[center]", "[]20[]10[]"));
        content.setOpaque(false);
        
        // 1. The Anahata Logo
        try {
            ImageIcon logo = new ImageIcon(ImageUtilities.loadImage("images/splash.png"));
            content.add(new JLabel(logo));
        } catch (Exception e) {
             JLabel fallback = new JLabel("Anahata ASI");
             fallback.setFont(new Font("SansSerif", Font.BOLD, 24));
             content.add(fallback);
        }
        
        // 2. The Progress Bar
        JProgressBar pb = new JProgressBar();
        pb.setIndeterminate(true);
        pb.setPreferredSize(new Dimension(300, 4));
        content.add(pb, "w 300!");
        
        // 3. The Status Label
        JLabel label = new JLabel("Initializing AGI Container...", SwingConstants.CENTER);
        label.setFont(label.getFont().deriveFont(Font.BOLD, 14f));
        label.setForeground(new Color(100, 100, 100));
        content.add(label);
        
        loadingPanel.add(content);
        add(loadingPanel, BorderLayout.CENTER);
        revalidate();
        repaint();
    }

    /**
     * Displays an error message in the component if session initialization fails.
     * 
     * @param message The error message to display.
     */
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
        if (agiPanel != null) {
            return agiPanel.getAgi();
        }
        if (sessionId != null) {
            // Try to find the agi in the container even if the panel isn't ready
            return AnahataInstaller.getContainer().getActiveAgis().stream()
                    .filter(c -> c.getConfig().getSessionId().equals(sessionId))
                    .findFirst().orElse(null);
        }
        return null;
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

        /**
         * Constructs a new Resolvable proxy for the given session ID.
         * 
         * @param sessionId The session ID to preserve.
         */
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
