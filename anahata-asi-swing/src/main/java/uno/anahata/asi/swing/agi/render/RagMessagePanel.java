/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.render;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Rectangle;
import javax.swing.BorderFactory;
import javax.swing.Scrollable;
import javax.swing.border.Border;
import lombok.NonNull;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * A specialized message panel for rendering messages in non-history UI components.
 * It suppresses pruning and removal controls by default and supports scrolling.
 * 
 * @author anahata
 */
public class RagMessagePanel extends AbstractMessagePanel<AbstractMessage> implements Scrollable {

    private final boolean renderPruneButtons;
    private final boolean renderRemoveButtons;

    /**
     * Constructs a new RagMessagePanel.
     * 
     * @param agiPanel The parent agi panel.
     * @param message The message to render.
     * @param renderPruneButtons Whether to render pruning controls.
     * @param renderRemoveButtons Whether to render remove controls.
     */
    public RagMessagePanel(@NonNull AgiPanel agiPanel, @NonNull AbstractMessage message, 
                             boolean renderPruneButtons, boolean renderRemoveButtons) {
        super(agiPanel, message);
        this.renderPruneButtons = renderPruneButtons;
        this.renderRemoveButtons = renderRemoveButtons;
        // Immediate update of buttons after super constructor
        updateHeaderButtons();
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
        return agiConfig.getTheme().getHeaderStartColor(message.getRole());
    }

    @Override
    protected Color getHeaderEndColor() {
        return agiConfig.getTheme().getHeaderEndColor(message.getRole());
    }

    @Override
    protected Color getHeaderForegroundColor() {
        return agiConfig.getTheme().getHeaderForegroundColor(message.getRole());
    }

    @Override
    protected Border getMessageBorder() {
        return BorderFactory.createLineBorder(agiConfig.getTheme().getBorderColor(message.getRole()), 1, true);
    }
}
