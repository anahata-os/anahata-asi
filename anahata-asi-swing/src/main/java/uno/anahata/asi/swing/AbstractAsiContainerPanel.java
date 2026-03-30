/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing;

import java.io.File;
import java.nio.file.Path;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

import java.awt.BorderLayout;
import java.awt.event.HierarchyEvent;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JToolBar;
import javax.swing.Timer;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.CancelIcon;
import uno.anahata.asi.swing.icons.LoadSessionIcon;

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

        toolBar.add(Box.createHorizontalGlue());
        
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

        // 2. Setup Refresh Timer
        this.refreshTimer = new Timer(1000, e -> {
            if (isShowing()) {
                refreshView();
                updateButtonState();
            }
        });

        setLayout(new BorderLayout());
        add(toolBar, BorderLayout.NORTH);
        
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
            showPreferences();
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
        AsiContainerPreferencesPanel prefsPanel = new AsiContainerPreferencesPanel(asiContainer);
        JOptionPane.showOptionDialog(this, prefsPanel, "ASI Container Preferences", 
                JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE, 
                new SettingsIcon(32), new Object[]{"Close"}, "Close");
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
