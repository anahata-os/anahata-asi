/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Rectangle;
import javax.swing.BorderFactory;
import javax.swing.Scrollable;
import javax.swing.SwingConstants;
import javax.swing.border.Border;
import lombok.NonNull;
import uno.anahata.ai.model.core.InputUserMessage;
import uno.anahata.ai.swing.chat.render.AbstractMessagePanel;

/**
 * A panel that provides a live, read-only preview of the user's input message,
 * including markdown rendering of the text part and a display of any attached
 * blob parts.
 * <p>
 * This component implements {@link Scrollable} to ensure it behaves correctly
 * within a JScrollPane, stretching to fill the viewport height if necessary.
 *
 * @author pablo
 */
public class UserInputMessagePanel extends AbstractMessagePanel<InputUserMessage> implements Scrollable {

    /**
     * Constructs a new InputMessagePanel.
     *
     * @param chatPanel The parent chat panel.
     * @param message The mutable message instance.
     */
    public UserInputMessagePanel(@NonNull ChatPanel chatPanel, @NonNull InputUserMessage message) {
        super(chatPanel, message);
    }

    @Override
    protected Color getHeaderStartColor() {
        return chatConfig.getTheme().getUserHeaderBg();
    }

    @Override
    protected Color getHeaderEndColor() {
        return chatConfig.getTheme().getUserContentBg();
    }

    @Override
    protected Color getHeaderForegroundColor() {
        return chatConfig.getTheme().getUserHeaderFg();
    }

    @Override
    protected Border getMessageBorder() {
        return BorderFactory.createLineBorder(chatConfig.getTheme().getUserBorder(), 2, true);
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
