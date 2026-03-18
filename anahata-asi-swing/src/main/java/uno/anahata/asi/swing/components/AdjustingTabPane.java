/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.components;

import java.awt.Component;
import java.awt.Dimension;
import javax.swing.JTabbedPane;

/**
 * A specialized JTabbedPane that adjusts its preferred and minimum height to match 
 * the currently selected component. 
 * <p>
 * This ensures that when placed inside a ScrollablePanel or a vertical BoxLayout,
 * the tab pane only occupies the space needed by the active tab.
 * </p>
 * 
 * @author anahata
 */
public class AdjustingTabPane extends JTabbedPane {
    
    private final int minHeight;

    /**
     * Constructs a new AdjustingTabPane.
     * @param minHeight The minimum height to maintain.
     */
    public AdjustingTabPane(int minHeight) {
        this.minHeight = minHeight;
        addChangeListener(e -> refresh());
    }

    @Override
    public void addTab(String title, Component component) {
        super.addTab(title, component);
        refresh();
    }

    /**
     * Forces a re-layout of the component and its ancestors.
     */
    public void refresh() {
        revalidate();
        repaint();
        if (getParent() != null) {
            getParent().revalidate();
            getParent().repaint();
        }
    }

    /** {@inheritDoc} */
    @Override
    public Dimension getPreferredSize() {
        return calculateAdjustedSize(super.getPreferredSize());
    }

    /** {@inheritDoc} */
    @Override
    public Dimension getMinimumSize() {
        return calculateAdjustedSize(super.getMinimumSize());
    }

    private Dimension calculateAdjustedSize(Dimension original) {
        Component c = getSelectedComponent();
        if (c != null) {
            Dimension d = c.getPreferredSize();
            // 45px covers the tab headers and a bit of margin
            int targetHeight = Math.max(minHeight, d.height + 45);
            return new Dimension(original.width, targetHeight);
        }
        return new Dimension(original.width, minHeight);
    }
}
