/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.render;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Rectangle;
import javax.swing.BorderFactory;
import javax.swing.Scrollable;
import javax.swing.border.Border;
import lombok.NonNull;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * A specialized message panel for use in the {@link OtherMessageViewer}.
 * It implements {@link Scrollable} and allows disabling pruning and removal controls.
 * 
 * @author anahata
 */
public class OtherMessagePanel extends AbstractMessagePanel<AbstractMessage> implements Scrollable {

    private final boolean renderPruneButtons;
    private final boolean renderRemoveButtons;

    /**
     * Constructs a new OtherMessagePanel.
     * 
     * @param chatPanel The parent chat panel.
     * @param message The message to render.
     * @param renderPruneButtons Whether to render pruning controls.
     * @param renderRemoveButtons Whether to render remove controls.
     */
    public OtherMessagePanel(@NonNull ChatPanel chatPanel, @NonNull AbstractMessage message, 
                             boolean renderPruneButtons, boolean renderRemoveButtons) {
        super(chatPanel, message);
        this.renderPruneButtons = renderPruneButtons;
        this.renderRemoveButtons = renderRemoveButtons;
    }

    @Override
    public boolean isRenderPruneButtons() {
        return renderPruneButtons;
    }

    @Override
    public boolean isRenderRemoveButtons() {
        return renderRemoveButtons;
    }

    @Override
    public Dimension getPreferredScrollableViewportSize() {
        return getPreferredSize();
    }

    @Override
    public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation, int direction) {
        return 16;
    }

    @Override
    public int getScrollableBlockIncrement(Rectangle visibleRect, int orientation, int direction) {
        return 16;
    }

    @Override
    public boolean getScrollableTracksViewportWidth() {
        return true;
    }

    @Override
    public boolean getScrollableTracksViewportHeight() {
        return false;
    }

    @Override
    protected Color getHeaderStartColor() {
        return chatConfig.getTheme().getHeaderStartColor(message.getRole());
    }

    @Override
    protected Color getHeaderEndColor() {
        return chatConfig.getTheme().getHeaderEndColor(message.getRole());
    }

    @Override
    protected Color getHeaderForegroundColor() {
        return chatConfig.getTheme().getHeaderForegroundColor(message.getRole());
    }

    @Override
    protected Border getMessageBorder() {
        return BorderFactory.createLineBorder(chatConfig.getTheme().getBorderColor(message.getRole()), 1, true);
    }
}
