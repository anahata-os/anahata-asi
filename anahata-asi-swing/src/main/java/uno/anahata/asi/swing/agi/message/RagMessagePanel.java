/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.message;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Rectangle;
import javax.swing.BorderFactory;
import javax.swing.Scrollable;
import javax.swing.border.Border;
import lombok.NonNull;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * A specialized message panel for rendering messages in non-history UI components.
 * It suppresses pruning and removal controls by default and supports scrolling.
 * 
 * @author anahata
 */
public class RagMessagePanel extends AbstractMessagePanel<AbstractMessage> implements Scrollable {

    /** Whether to render pruning controls. */
    private final boolean renderPruneButtons;
    /** Whether to render remove controls. */
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

    /** 
     * {@inheritDoc} 
     * <p>Returns whether pruning buttons should be rendered.</p>
     */
    @Override
    public boolean isRenderPruneButtons() {
        return renderPruneButtons;
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns whether remove buttons should be rendered.</p>
     */
    @Override
    public boolean isRenderRemoveButtons() {
        return renderRemoveButtons;
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the preferred size for the scrollable viewport.</p>
     */
    @Override
    public Dimension getPreferredScrollableViewportSize() {
        return getPreferredSize();
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the unit increment for scrolling.</p>
     */
    @Override
    public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation, int direction) {
        return 16;
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the block increment for scrolling.</p>
     */
    @Override
    public int getScrollableBlockIncrement(Rectangle visibleRect, int orientation, int direction) {
        return 16;
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns whether the scrollable tracks viewport width.</p>
     */
    @Override
    public boolean getScrollableTracksViewportWidth() {
        return true;
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns whether the scrollable tracks viewport height.</p>
     */
    @Override
    public boolean getScrollableTracksViewportHeight() {
        return false;
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns an empty string for the header suffix, as RAG messages are not part of the tracked history depth.</p>
     */
    @Override
    protected String getHeaderSuffix() {
        return "";
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the header start color from the theme.</p>
     */
    @Override
    protected Color getHeaderStartColor() {
        return agiConfig.getTheme().getHeaderStartColor(message.getRole());
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the header end color from the theme.</p>
     */
    @Override
    protected Color getHeaderEndColor() {
        return agiConfig.getTheme().getHeaderEndColor(message.getRole());
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the header foreground color from the theme.</p>
     */
    @Override
    protected Color getHeaderForegroundColor() {
        return agiConfig.getTheme().getHeaderForegroundColor(message.getRole());
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the message border from the theme.</p>
     */
    @Override
    protected Border getMessageBorder() {
        return BorderFactory.createLineBorder(agiConfig.getTheme().getBorderColor(message.getRole()), 1, true);
    }
}
