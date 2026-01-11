/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.components;

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
import uno.anahata.ai.swing.icons.CopyIcon;
import uno.anahata.ai.swing.internal.SwingUtils;

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

    @Setter
    private boolean trackViewportWidth = true;

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
