/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Rectangle;
import javax.swing.BorderFactory;
import javax.swing.Scrollable;
import javax.swing.SwingConstants;
import javax.swing.border.Border;
import lombok.NonNull;
import uno.anahata.asi.model.core.InputUserMessage;
import uno.anahata.asi.swing.agi.render.AbstractMessagePanel;

/**
 * A panel that provides a live, read-only preview of the user's input message,
 * including markdown rendering of the text part and a display of any attached
 * blob parts.
 * <p>
 * This component implements {@link Scrollable} to ensure it behaves correctly
 * within a JScrollPane, stretching to fill the viewport height if necessary.
 *
 * @author anahata
 */
public class UserInputMessagePanel extends AbstractMessagePanel<InputUserMessage> implements Scrollable {

    /**
     * Constructs a new InputMessagePanel.
     *
     * @param agiPanel The parent agi panel.
     * @param message The mutable message instance.
     */
    public UserInputMessagePanel(@NonNull AgiPanel agiPanel, @NonNull InputUserMessage message) {
        super(agiPanel, message);
    }

    @Override
    protected Color getHeaderStartColor() {
        return agiConfig.getTheme().getUserHeaderBg();
    }

    @Override
    protected Color getHeaderEndColor() {
        return agiConfig.getTheme().getUserContentBg();
    }

    @Override
    protected Color getHeaderForegroundColor() {
        return agiConfig.getTheme().getUserHeaderFg();
    }

    @Override
    protected Border getMessageBorder() {
        return BorderFactory.createLineBorder(agiConfig.getTheme().getUserBorder(), 2, true);
    }

    // --- Scrollable Implementation ---

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
        return (orientation == SwingConstants.VERTICAL) ? visibleRect.height : visibleRect.width;
    }

    @Override
    public boolean getScrollableTracksViewportWidth() {
        return true;
    }

    @Override
    public boolean getScrollableTracksViewportHeight() {
        // Stretch to fill the viewport height if the panel is smaller than the viewport
        if (getParent() instanceof javax.swing.JViewport viewport) {
            return viewport.getHeight() > getPreferredSize().height;
        }
        return false;
    }
}
