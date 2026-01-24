/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat;

import java.awt.BorderLayout;
import java.awt.event.HierarchyEvent;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JToolBar;
import javax.swing.Timer;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.RestartIcon;

/**
 * A base abstract class for panels that manage a collection of AI chat sessions.
 * It provides a standard toolbar with common actions (New, Close, Dispose) and
 * a background refresh mechanism.
 * 
 * @author anahata
 */
public abstract class AbstractAsiContainerPanel extends JPanel implements SessionController {
    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractAsiContainerPanel.class);

    /** The application-wide ASI container. */
    @Getter
    protected final AsiContainer asiContainer;
    
    /** The controller for handling session actions. */
    @Getter @Setter
    protected SessionController controller;
    
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
    public AbstractAsiContainerPanel(@NonNull AsiContainer container) {
        this.asiContainer = container;
        
        // 1. Setup Toolbar
        this.toolBar = new JToolBar();
        toolBar.setFloatable(false);

        JButton newButton = new JButton("New", new RestartIcon(16));
        newButton.setToolTipText("Create a new AI session");
        newButton.addActionListener(e -> createNew());
        toolBar.add(newButton);

        closeButton = new JButton("Close");
        closeButton.setToolTipText("Close the selected AI session window");
        closeButton.addActionListener(e -> {
            Chat chat = getSelectedChat();
            if (chat != null) close(chat);
        });
        closeButton.setEnabled(false);
        toolBar.add(closeButton);
        
        toolBar.add(Box.createHorizontalGlue());

        disposeButton = new JButton("Dispose", new DeleteIcon(16));
        disposeButton.setToolTipText("Permanently dispose of the selected session");
        disposeButton.addActionListener(e -> {
            Chat chat = getSelectedChat();
            if (chat != null) dispose(chat);
        });
        disposeButton.setEnabled(false);
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
                if (isShowing()) startRefresh();
                else stopRefresh();
            }
        });
    }

    @Override
    public void focus(@NonNull Chat chat) {
        if (controller != null) controller.focus(chat);
    }

    @Override
    public void close(@NonNull Chat chat) {
        if (controller != null) controller.close(chat);
    }

    @Override
    public void dispose(@NonNull Chat chat) {
        if (controller != null) controller.dispose(chat);
    }

    @Override
    public void createNew() {
        if (controller != null) controller.createNew();
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
        Chat selected = getSelectedChat();
        boolean isSelected = selected != null;
        disposeButton.setEnabled(isSelected);
        closeButton.setEnabled(isSelected);
    }

    /**
     * Refreshes the specific view implementation (e.g., table or cards).
     */
    protected abstract void refreshView();

    /**
     * Gets the currently selected chat session in the view.
     * 
     * @return The selected chat, or null if none.
     */
    protected abstract Chat getSelectedChat();
}
