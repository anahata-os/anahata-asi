/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.render;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Rectangle;
import javax.swing.BorderFactory;
import javax.swing.JScrollPane;
import javax.swing.Scrollable;
import javax.swing.border.Border;
import lombok.NonNull;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * A specialized scroll pane for rendering any {@link AbstractMessage}. 
 * This is useful for displaying messages in secondary UI locations 
 * like toolkit details or system instruction previews.
 *
 * @author anahata
 */
public class OtherMessagePanel extends JScrollPane {

    private final ChatPanel chatPanel;
    private final AbstractMessage message;
    private final AbstractMessagePanel<AbstractMessage> messagePanel;

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
        this.chatPanel = chatPanel;
        this.message = message;

        this.messagePanel = createMessagePanel(message);
        this.messagePanel.setRenderPruneButtons(renderPruneButtons);
        this.messagePanel.setRenderRemoveButtons(renderRemoveButtons);

        setViewportView(messagePanel);
        setBorder(null);
        setOpaque(false);
        getViewport().setOpaque(false);
        setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        getVerticalScrollBar().setUnitIncrement(16);
    }

    private AbstractMessagePanel<AbstractMessage> createMessagePanel(AbstractMessage message) {
        class ScrollableMessagePanel extends AbstractMessagePanel<AbstractMessage> implements Scrollable {
            public ScrollableMessagePanel(ChatPanel chatPanel, AbstractMessage message) {
                super(chatPanel, message);
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
        return new ScrollableMessagePanel(chatPanel, message);
    }
    
    /**
     * Triggers a re-render of the underlying message panel.
     */
    public void render() {
        messagePanel.render();
    }
}
