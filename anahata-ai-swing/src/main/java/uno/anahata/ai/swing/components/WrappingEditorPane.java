/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.components;

import java.awt.Dimension;
import java.awt.Rectangle;
import javax.swing.JEditorPane;
import javax.swing.Scrollable;
import lombok.Setter;
import uno.anahata.ai.swing.internal.SwingUtils;

/**
 * A JEditorPane subclass that implements the {@link Scrollable} interface to
 * ensure correct line wrapping and horizontal scrolling behavior when placed
 * inside a JScrollPane.
 * <p>
 * By default, it forces {@code getScrollableTracksViewportWidth()} to return
 * {@code true}, which is the key to making HTML content wrap correctly.
 *
 * @author pablo
 */
public class WrappingEditorPane extends JEditorPane implements Scrollable {

    @Setter
    private boolean trackViewportWidth = true;

    public WrappingEditorPane() {
        // Redispatch mouse wheel events to the parent scroll pane to ensure
        // vertical scrolling works even when the mouse is over this component.
        addMouseWheelListener(e -> SwingUtils.redispatchMouseWheelEvent(this, e));
    }

    @Override
    public boolean getScrollableTracksViewportWidth() {
        return trackViewportWidth;
    }

    @Override
    public Dimension getPreferredScrollableViewportSize() {
        return getPreferredSize();
    }

    @Override
    public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation, int direction) {
        return 24;
    }

    @Override
    public int getScrollableBlockIncrement(Rectangle visibleRect, int orientation, int direction) {
        return visibleRect.height;
    }

    @Override
    public boolean getScrollableTracksViewportHeight() {
        return false;
    }
}
