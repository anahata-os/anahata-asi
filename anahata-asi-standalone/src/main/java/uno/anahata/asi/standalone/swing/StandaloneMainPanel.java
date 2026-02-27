/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.standalone.swing;

import java.awt.BorderLayout;
import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.AsiSwitcherContainerPanel;
import uno.anahata.asi.swing.agi.SessionController;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;

/**
 * The main container for the Anahata AI Swing UI, managing multiple agi sessions.
 * It provides a session list and a tabbed area for active agis.
 * 
 * @author gemini-3-flash-preview
 */
@Slf4j
public class StandaloneMainPanel extends JPanel implements SessionController {

    /** The parent ASI container managing the global state. */
    private final StandaloneAsiContainer asiContainer;
    
    /** The sidebar panel for switching between active sessions. */
    private final AsiSwitcherContainerPanel asiContainerPanel;
    
    /** The central tabbed pane for displaying active agi conversations. */
    private final JTabbedPane tabbedPane;
    
    /** The listener for changes in the container's active agis list. */
    private final EdtPropertyChangeListener asiListener;

    /**
     * Constructs a new MainPanel.
     * 
     * @param container The standalone ASI container.
     */
    public StandaloneMainPanel(StandaloneAsiContainer container) {
        this.asiContainer = container;
        
        setLayout(new BorderLayout());

        asiContainerPanel = new AsiSwitcherContainerPanel(container);
        asiContainerPanel.setController(this);

        tabbedPane = new JTabbedPane();
        tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
        
        // Enable closable tabs via FlatLaf properties
        tabbedPane.putClientProperty("JTabbedPane.tabClosable", true);
        tabbedPane.putClientProperty("JTabbedPane.tabCloseCallback", (BiConsumer<JTabbedPane, Integer>) (tabPane, tabIndex) -> {
            Component comp = tabPane.getComponentAt(tabIndex);
            if (comp instanceof AgiPanel agiPanel) {
                close(agiPanel.getAgi());
            }
        });

        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, asiContainerPanel, tabbedPane);
        splitPane.setDividerLocation(300);
        splitPane.setOneTouchExpandable(true);
        add(splitPane, BorderLayout.CENTER);
        
        this.asiListener = new EdtPropertyChangeListener(this, container, "activeAgis", this::handleAsiChange);
    }

    /**
     * Starts the background refresh of the session list and loads persisted sessions.
     * If no sessions are loaded, a new empty agi is created.
     */
    public void start() {
        asiContainerPanel.startRefresh();
        
        // Load persisted sessions from disk
        asiContainer.loadSessions();
        
        List<Agi> activeAgis = asiContainer.getActiveAgis();
        if (activeAgis.isEmpty()) {
            log.info("No active sessions found. Creating a new empty agi.");
            createNew();
        } else {
            // Sync existing agis - use a copy to avoid ConcurrentModificationException
            for (Agi agi : new ArrayList<>(activeAgis)) {
                focus(agi);
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Ensures the agi has a corresponding tab and selects it.
     * 
     * @param agi The agi session to focus.
     */
    @Override
    public void focus(@NonNull Agi agi) {
        String id = agi.getConfig().getSessionId();
        log.info("Focusing session: {}", id);
        
        // Check if we already have a tab for this agi
        int tabIndex = -1;
        for (int i = 0; i < tabbedPane.getTabCount(); i++) {
            Component comp = tabbedPane.getTabCount() > i ? tabbedPane.getComponentAt(i) : null;
            if (comp != null && id.equals(comp.getName())) {
                tabIndex = i;
                break;
            }
        }

        if (tabIndex == -1) {
            log.info("Creating new tab for session: {}", id);
            // Use the existing agi instance instead of creating a new one
            AgiPanel panel = new AgiPanel(agi);
            panel.setName(id);
            panel.initComponents();
            
            tabbedPane.addTab(agi.getDisplayName(), panel);
            tabIndex = tabbedPane.getTabCount() - 1;
            
            // Listen for nickname changes to update the tab title
            new EdtPropertyChangeListener(this, agi, "nickname", this::handleNicknameChange);
        }

        tabbedPane.setSelectedIndex(tabIndex);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Removes the tab associated with the agi session.
     * 
     * @param agi The agi session to close.
     */
    @Override
    public void close(@NonNull Agi agi) {
        String id = agi.getConfig().getSessionId();
        log.info("Closing tab for session: {}", id);
        for (int i = 0; i < tabbedPane.getTabCount(); i++) {
            if (id.equals(tabbedPane.getComponentAt(i).getName())) {
                tabbedPane.removeTabAt(i);
                // EdtPropertyChangeListener will be GC'd as it's not strongly held by the agi
                break;
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Closes the tab and delegates the permanent disposal of the session to the container.
     * 
     * @param agi The agi session to dispose.
     */
    @Override
    public void dispose(@NonNull Agi agi) {
        log.info("Disposing session: {}", agi.getConfig().getSessionId());
        close(agi);
        asiContainer.dispose(agi);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Creates a new {@link Agi} with a {@link StandaloneAgiConfig} and focuses it.
     */
    @Override
    public void createNew() {
        log.info("Creating new session...");
        // Agi constructor registers itself in AsiContainer, which triggers property change
        Agi agi = new Agi(new StandaloneAgiConfig(asiContainer));
        focus(agi);
    }

    /**
     * Handles updates to a agi's nickname by updating the corresponding tab title.
     * 
     * @param evt The property change event for "nickname".
     */
    private void handleNicknameChange(PropertyChangeEvent evt) {
        Agi agi = (Agi) evt.getSource();
        String id = agi.getConfig().getSessionId();
        String newDisplayName = agi.getDisplayName();
        
        for (int i = 0; i < tabbedPane.getTabCount(); i++) {
            if (id.equals(tabbedPane.getComponentAt(i).getName())) {
                tabbedPane.setTitleAt(i, newDisplayName);
                break;
            }
        }
    }

    /**
     * Handles changes to the container's active agis list, syncing the UI tabs.
     * 
     * @param evt The property change event for "activeAgis".
     */
    private void handleAsiChange(PropertyChangeEvent evt) {
        List<Agi> oldList = (List<Agi>) evt.getOldValue();
        List<Agi> newList = (List<Agi>) evt.getNewValue();
        
        // Handle additions
        if (newList != null) {
            for (Agi agi : new ArrayList<>(newList)) {
                if (oldList == null || !oldList.contains(agi)) {
                    focus(agi);
                }
            }
        }
        
        // Handle removals
        if (oldList != null) {
            for (Agi agi : new ArrayList<>(oldList)) {
                if (newList == null || !newList.contains(agi)) {
                    close(agi);
                }
            }
        }
    }
}
