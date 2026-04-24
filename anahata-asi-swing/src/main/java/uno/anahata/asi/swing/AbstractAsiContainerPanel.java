/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing;


import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.HierarchyEvent;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import net.miginfocom.swing.MigLayout;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.CancelIcon;
import uno.anahata.asi.swing.icons.SaveIcon;
import uno.anahata.asi.swing.icons.LoadSessionIcon;

import uno.anahata.asi.swing.agi.status.TaskStatusComponent;
import uno.anahata.asi.swing.icons.RestartIcon;
import uno.anahata.asi.swing.icons.SettingsIcon;

/**
 * A base abstract class for panels that manage a collection of AI agi sessions.
 * It provides a standard toolbar with common actions (New, Close, Dispose) and
 * a background refresh mechanism.
 * 
 * @author anahata
 */
@Slf4j
public abstract class AbstractAsiContainerPanel extends JPanel {

    /** The application-wide ASI container. */
    @Getter
    protected final AbstractSwingAsiContainer asiContainer;
    
    /** The toolbar containing session actions. */
    protected final JToolBar toolBar;
    /** Button to close the selected session's window. */
    protected final JButton closeButton;
    /** Button to permanently dispose of the selected session. */
    protected final JButton disposeButton;
    
    /** Timer for periodic UI refreshes. */
    private final Timer refreshTimer;

    /**
     * Constructs a new container panel.
     * 
     * @param container The ASI container.
     */
    public AbstractAsiContainerPanel(@NonNull AbstractSwingAsiContainer container) {
        this.asiContainer = container;
        
        // 1. Setup Toolbar
        this.toolBar = new JToolBar();
        toolBar.setFloatable(false);

        JButton newButton = new JButton("New", new RestartIcon(16));
        newButton.setToolTipText("Create a new AI session");
        newButton.addActionListener(e -> createNew());
        toolBar.add(newButton);

        JButton importButton = new JButton("Import", new LoadSessionIcon(16));
        importButton.setToolTipText("Import a previously saved AI session");
        importButton.addActionListener(e -> importSession());
        toolBar.add(importButton);

        JButton settingsBtn = new JButton("Preferences", new SettingsIcon(16));
        settingsBtn.setToolTipText("Configure global ASI settings and API keys");
        settingsBtn.addActionListener(e -> showPreferences());
        toolBar.add(settingsBtn);

        toolBar.add(Box.createHorizontalGlue());

        closeButton = new JButton("Close", new CancelIcon(16));
        closeButton.setToolTipText("Close the selected AI session window");
        closeButton.addActionListener(e -> {
            Agi agi = getSelectedAgi();
            if (agi != null) close(agi);
        });
        closeButton.setEnabled(false);
        toolBar.add(closeButton);
        
        disposeButton = new JButton("Dispose", new DeleteIcon(16));
        disposeButton.setToolTipText("Permanently delete the selected AI session");
        disposeButton.addActionListener(e -> {
            Agi agi = getSelectedAgi();
            if (agi != null) dispose(agi);
        });
        toolBar.add(disposeButton);

        // 2. Setup Header Wrapper (Toolbar + Status Row)
        JPanel headerWrapper = new JPanel(new MigLayout("ins 0, fillx, gap 0", "[grow, fill]", "[][]"));
        headerWrapper.setOpaque(false);
        headerWrapper.add(toolBar, "wrap");
        
        TaskStatusComponent taskMonitor = new TaskStatusComponent(asiContainer);
        headerWrapper.add(taskMonitor, "center, gaptop 2, gapbottom 2");

        // 3. Setup Refresh Timer
        this.refreshTimer = new Timer(1000, e -> {
            if (isShowing()) {
                refreshView();
                updateButtonState();
            }
        });

        setLayout(new BorderLayout());
        add(headerWrapper, BorderLayout.NORTH);
        
        // Auto-start/stop refresh based on visibility
        addHierarchyListener(e -> {
            if ((e.getChangeFlags() & HierarchyEvent.SHOWING_CHANGED) != 0) {
                if (isShowing()) {
                    startRefresh();
                } else {
                    stopRefresh();
                }
            }
        });
    }

    /** 
     * Authoritatively requests focus for the given agi session via the container.
     * 
     * @param agi The agi session to focus.
     */
    public void focus(@NonNull Agi agi) {
        asiContainer.open(agi);
    }

    /** 
     * Authoritatively requests the closure of the given agi session via the container.
     * 
     * @param agi The agi session to close.
     */
    public void close(@NonNull Agi agi) {
        asiContainer.close(agi);
    }

    /** 
     * Authoritatively requests the disposal of the given agi session via the container.
     * 
     * @param agi The agi session to dispose.
     */
    public void dispose(@NonNull Agi agi) {
        asiContainer.dispose(agi);
    }

    /** 
     * Authoritatively creates a new agi session via the container.
     * <p>
     * <b>Operational Guard:</b> If no API keys are configured, this method 
     * alerts the user and opens the Preferences dashboard instead of 
     * spawning a non-functional session.
     * </p>
     */
    public void createNew() {
        if (!asiContainer.hasAnyApiKeysConfigured()) {
            JOptionPane.showMessageDialog(this, 
                    "<html>Welcome to the Anahata Java Renaissance!<br><br>" +
                    "To begin, you need to configure at least one API key for an AI provider.<br>" +
                    "I am opening the <b>Preferences</b> dashboard for you now.</html>", 
                    "Setup Required", JOptionPane.INFORMATION_MESSAGE);
            showPreferences(3);
            return;
        }
        asiContainer.createNewAgi();
    }

    /** 
     * Invokes the shared Swing import UI from the container.
     */
    public void importSession() {
        asiContainer.importSessionWithUI(this);
    }

    /**
     * Displays the global ASI preferences dashboard in a modal dialog.
     */
    public void showPreferences() {
        showPreferences(0);
    }

    /**
     * Displays the global ASI preferences dashboard with a specific tab selected.
     * 
     * @param initialTabIndex The index of the tab to open.
     */
    public void showPreferences(int initialTabIndex) {
        AsiContainerPreferencesPanel prefsPanel = new AsiContainerPreferencesPanel(this, initialTabIndex);
        
        JButton saveBtn = new JButton("Save", new SaveIcon(16));
        JButton cancelBtn = new JButton("Cancel", new CancelIcon(16));
        
        Object[] options = {saveBtn, cancelBtn};
        
        JOptionPane pane = new JOptionPane(prefsPanel, JOptionPane.PLAIN_MESSAGE, JOptionPane.DEFAULT_OPTION, new SettingsIcon(32), options, null);
        javax.swing.JDialog dialog = pane.createDialog(SwingUtilities.getWindowAncestor(this), "ASI Container Preferences");
        dialog.setLocationRelativeTo(SwingUtilities.getWindowAncestor(this));
        
        saveBtn.addActionListener(e -> {
            try {
                prefsPanel.save();
                dialog.dispose();
                JOptionPane.showMessageDialog(this, "Global configuration saved and applied.", "Success", JOptionPane.INFORMATION_MESSAGE);
            } catch (Exception ex) {
                log.error("Failed to save preferences", ex);
                JOptionPane.showMessageDialog(dialog, "Failed to save: " + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
            }
        });
        
        cancelBtn.addActionListener(e -> dialog.dispose());
        
        dialog.setVisible(true);
    }



    /**
     * Sets whether the toolbar is visible.
     * 
     * @param visible true to show the toolbar, false to hide it.
     */
    public void setToolBarVisible(boolean visible) {
        toolBar.setVisible(visible);
    }

    /**
     * Starts the background refresh timer.
     */
    public void startRefresh() {
        if (!refreshTimer.isRunning()) {
            refreshTimer.start();
        }
    }

    /**
     * Stops the background refresh timer.
     */
    public void stopRefresh() {
        refreshTimer.stop();
    }

    /**
     * Updates the enabled state of toolbar buttons based on the current selection.
     */
    protected void updateButtonState() {
        Agi selected = getSelectedAgi();
        boolean isSelected = selected != null;
        disposeButton.setEnabled(isSelected);
        closeButton.setEnabled(isSelected);
    }

    /**
     * Refreshes the specific view implementation (e.g., table or cards).
     */
    protected abstract void refreshView();

    /**
     * Gets the currently selected agi session in the view.
     * 
     * @return The selected agi, or null if none.
     */
    protected abstract Agi getSelectedAgi();
}
