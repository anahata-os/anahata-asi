/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.components;

import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JEditorPane;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.Scrollable;
import javax.swing.event.HyperlinkEvent;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.icons.CopyIcon;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * A JEditorPane subclass that implements the {@link Scrollable} interface to
 * ensure correct line wrapping and horizontal scrolling behavior when placed
 * inside a JScrollPane.
 * <p>
 * By default, it forces {@code getScrollableTracksViewportWidth()} to return
 * {@code true}, which is the key to making HTML content wrap correctly.
 *
 * @author anahata
 */
@Slf4j
public class WrappingEditorPane extends JEditorPane implements Scrollable {

    /**
     * Whether this editor pane should track the viewport's width to force 
     * line wrapping.
     * <p>
     * When {@code true}, horizontal scrolling is disabled and content is 
     * wrapped within the viewport's bounds.
     * </p>
     */
    @Setter
    private boolean trackViewportWidth = true;

    /**
     * Constructs a new WrappingEditorPane.
     * <p>
     * Initializes the component with mouse wheel redispatching, hyperlink 
     * activation, and a context menu for copying text.
     * </p>
     */
    public WrappingEditorPane() {
        // Redispatch mouse wheel events to the parent scroll pane to ensure
        // vertical scrolling works even when the mouse is over this component.
        addMouseWheelListener(e -> SwingUtils.redispatchMouseWheelEvent(this, e));
        
        // Enable hyperlink activation
        addHyperlinkListener(e -> {
            if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
                try {
                    Desktop.getDesktop().browse(e.getURL().toURI());
                } catch (Exception ex) {
                    log.error("Failed to open link: {}", e.getURL(), ex);
                }
            }
        });

        // Add context menu for copying selected text
        addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                if (e.isPopupTrigger()) showPopup(e);
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                if (e.isPopupTrigger()) showPopup(e);
            }

            private void showPopup(MouseEvent e) {
                String selectedText = getSelectedText();
                if (selectedText != null && !selectedText.isEmpty()) {
                    JPopupMenu popup = new JPopupMenu();
                    JMenuItem copyItem = new JMenuItem("Copy", new CopyIcon(14));
                    copyItem.addActionListener(ae -> copy());
                    popup.add(copyItem);
                    popup.show(WrappingEditorPane.this, e.getX(), e.getY());
                }
            }
        });
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the configured {@link #trackViewportWidth} state. When enabled, 
     * this is what triggers HTML line wrapping in a scroll pane.</p> 
     */
    public boolean getScrollableTracksViewportWidth() {
        return trackViewportWidth;
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the component's preferred size, allowing the parent scroll 
     * pane to manage the layout.</p> 
     */
    public Dimension getPreferredScrollableViewportSize() {
        return getPreferredSize();
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns a fixed increment of 24 pixels for smooth vertical scrolling.</p> 
     */
    public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation, int direction) {
        return 24;
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the visible height of the rectangle to support page-based scrolling.</p> 
     */
    public int getScrollableBlockIncrement(Rectangle visibleRect, int orientation, int direction) {
        return visibleRect.height;
    }

    /** 
     * {@inheritDoc} 
     * <p>Always returns {@code false} as this component is designed for 
     * vertical scrolling with dynamic height.</p> 
     */
    public boolean getScrollableTracksViewportHeight() {
        return false;
    }
}
