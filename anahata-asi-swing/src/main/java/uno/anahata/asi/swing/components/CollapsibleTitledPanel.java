/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.components;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import org.jdesktop.swingx.JXTitledPanel;

/**
 * A base class extending {@link JXTitledPanel} that provides robust, 
 * Look-and-Feel-agnostic header click detection for expand/collapse behavior.
 * @author anahata
 */
public abstract class CollapsibleTitledPanel extends JXTitledPanel {
    /**
     * The cached mouse listener instance attached to the header to trigger expand/collapse.
     */
    private MouseAdapter headerClickListener;

    /**
     * Lazy initializer for the header click listener.
     * This avoids null references during the superclass constructor execution
     * since subclass fields are not yet initialized when updateUI is first invoked.
     *
     * @return The non-null mouse listener instance.
     */
    private MouseAdapter getHeaderClickListener() {
        if (headerClickListener == null) {
            headerClickListener = new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    toggleExpanded();
                }
            };
        }
        return headerClickListener;
    }

    /**
     * Invoked when the user clicks the header.
     * Subclasses must implement their specific expand/collapse logic here.
     */
    protected abstract void toggleExpanded();

    /**
     * Look-and-Feel-agnostic resolver to find the actual title bar/header component
     * of the titled panel by finding the child that is not the content container.
     *
     * @return The header component, or null if not found.
     */
    public Component getHeaderComponent() {
        for (Component child : getComponents()) {
            if (child != getContentContainer()) {
                return child;
            }
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * <p>Overridden to dynamically re-attach the header click listener whenever the UI delegate is updated (e.g., during a Look and Feel change).</p>
     */
    @Override
    public void updateUI() {
        super.updateUI();
        Component header = getHeaderComponent();
        if (header != null) {
            header.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            header.removeMouseListener(getHeaderClickListener());
            header.addMouseListener(getHeaderClickListener());
        }
    }
}