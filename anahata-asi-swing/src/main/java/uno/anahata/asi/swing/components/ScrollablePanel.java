/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.components;

import java.awt.Dimension;
import java.awt.Rectangle;
import javax.swing.JPanel;
import javax.swing.Scrollable;
import javax.swing.SwingConstants;

/**
 * A flexible JPanel that implements the Scrollable interface, allowing for
 * configurable scrolling behavior. This is useful for panels placed in a
 * JScrollPane.
 *
 * @author anahata
 */
public class ScrollablePanel extends JPanel implements Scrollable {

    /**
     * Whether this panel's width should always match the viewport's width.
     * <p>
     * When {@code true}, horizontal scrolling is effectively disabled and 
     * components are expected to wrap or scale.
     * </p>
     */
    private boolean scrollableTracksViewportWidth = true;
    /**
     * Whether this panel's height should always match the viewport's height.
     * <p>
     * When {@code true}, vertical scrolling is effectively disabled.
     * </p>
     */
    private boolean scrollableTracksViewportHeight = false;

    /**
     * Default constructor. Initializes the panel for vertical scrolling,
     * which is the most common use case.
     */
    public ScrollablePanel() {
        this(SwingConstants.VERTICAL);
    }
    
    /**
     * Creates a new ScrollablePanel with a specific scrolling orientation.
     *
     * @param orientation The scrolling orientation, either SwingConstants.VERTICAL or SwingConstants.HORIZONTAL.
     */
    public ScrollablePanel(int orientation) {
        switch (orientation) {
            case SwingConstants.VERTICAL:
                this.scrollableTracksViewportWidth = true;
                this.scrollableTracksViewportHeight = false;
                break;
            case SwingConstants.HORIZONTAL:
                this.scrollableTracksViewportWidth = false;
                this.scrollableTracksViewportHeight = true;
                break;
            default:
                throw new IllegalArgumentException("Orientation must be either SwingConstants.VERTICAL or SwingConstants.HORIZONTAL");
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the component's preferred size as the viewport size, 
     * assuming the parent scroll pane will manage the actual visible area.</p> 
     */
    public Dimension getPreferredScrollableViewportSize() {
        return getPreferredSize();
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns a fixed increment of 24 pixels, optimized for smooth 
     * scrolling of chat and context components.</p> 
     */
    public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation, int direction) {
        return 24; // Optimized for faster scrolling in agi/context views
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the full height (or width) of the visible rectangle to 
     * support "page down/up" scrolling behavior.</p> 
     */
    public int getScrollableBlockIncrement(Rectangle visibleRect, int orientation, int direction) {
        return (orientation == SwingConstants.VERTICAL) ? visibleRect.height : visibleRect.width;
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the configured {@link #scrollableTracksViewportWidth} state.</p> 
     */
    public boolean getScrollableTracksViewportWidth() {
        return scrollableTracksViewportWidth;
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the configured {@link #scrollableTracksViewportHeight} state.</p> 
     */
    public boolean getScrollableTracksViewportHeight() {
        return scrollableTracksViewportHeight;
    }

    /**
     * Sets whether this panel should track the viewport's width.
     * @param scrollableTracksViewportWidth {@code true} to track width.
     */
    public void setScrollableTracksViewportWidth(boolean scrollableTracksViewportWidth) {
        this.scrollableTracksViewportWidth = scrollableTracksViewportWidth;
    }

    /**
     * Sets whether this panel should track the viewport's height.
     * @param scrollableTracksViewportHeight {@code true} to track height.
     */
    public void setScrollableTracksViewportHeight(boolean scrollableTracksViewportHeight) {
        this.scrollableTracksViewportHeight = scrollableTracksViewportHeight;
    }
}