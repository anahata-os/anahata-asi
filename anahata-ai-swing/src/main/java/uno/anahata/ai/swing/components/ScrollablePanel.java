package uno.anahata.ai.swing;

import java.awt.Dimension;
import java.awt.Rectangle;
import javax.swing.JPanel;
import javax.swing.Scrollable;
import javax.swing.SwingConstants;

/**
 * A flexible JPanel that implements the Scrollable interface, allowing for
 * configurable scrolling behavior. This is useful for panels placed in a
 * JScrollPane.
 */
public class ScrollablePanel extends JPanel implements Scrollable {

    private boolean scrollableTracksViewportWidth = true;
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

    @Override
    public Dimension getPreferredScrollableViewportSize() {
        return getPreferredSize();
    }

    @Override
    public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation, int direction) {
        return 24; // Optimized for faster scrolling in chat/context views
    }

    @Override
    public int getScrollableBlockIncrement(Rectangle visibleRect, int orientation, int direction) {
        return (orientation == SwingConstants.VERTICAL) ? visibleRect.height : visibleRect.width;
    }

    @Override
    public boolean getScrollableTracksViewportWidth() {
        return scrollableTracksViewportWidth;
    }

    @Override
    public boolean getScrollableTracksViewportHeight() {
        return scrollableTracksViewportHeight;
    }

    public void setScrollableTracksViewportWidth(boolean scrollableTracksViewportWidth) {
        this.scrollableTracksViewportWidth = scrollableTracksViewportWidth;
    }

    public void setScrollableTracksViewportHeight(boolean scrollableTracksViewportHeight) {
        this.scrollableTracksViewportHeight = scrollableTracksViewportHeight;
    }
}
