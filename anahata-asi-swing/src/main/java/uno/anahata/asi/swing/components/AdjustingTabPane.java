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
    
    /**
     * The absolute minimum height to maintain for this tab pane.
     * <p>
     * This ensures a baseline visibility even if the content of the 
     * selected tab is smaller or null.
     * </p>
     */
    private final int minHeight;

    /**
     * Constructs a new AdjustingTabPane.
     * @param minHeight The minimum height to maintain.
     */
    public AdjustingTabPane(int minHeight) {
        this.minHeight = minHeight;
        addChangeListener(e -> refresh());
    }

    /** 
     * {@inheritDoc} 
     * <p>Overridden to trigger a {@link #refresh()} whenever a new tab is added, 
     * ensuring the layout is updated to reflect the new tab's height.</p> 
     */
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

    /** 
     * {@inheritDoc} 
     * <p>Calculates the preferred size by adjusting the height to match the 
     * currently selected component's preferred height plus header overhead.</p> 
     */
    @Override
    public Dimension getPreferredSize() {
        return calculateAdjustedSize(super.getPreferredSize());
    }

    /** 
     * {@inheritDoc} 
     * <p>Ensures the minimum size respects the dynamic adjustment logic and 
     * the configured {@link #minHeight}.</p> 
     */
    @Override
    public Dimension getMinimumSize() {
        return calculateAdjustedSize(super.getMinimumSize());
    }

    /**
     * Calculates the dynamically adjusted dimension for the tab pane.
     * <p>
     * It extracts the height from the currently selected component and adds 
     * a constant offset (45px) for the tab headers. The resulting height 
     * is clamped to the minimum height.
     * </p>
     * @param original The original dimension to use as a template (mainly for width).
     * @return The adjusted dimension.
     */
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
