/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.BorderLayout;
import java.awt.Color;
import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.Border;
import lombok.NonNull;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.Role;
import uno.anahata.ai.swing.chat.ChatPanel;

/**
 * A specialized panel for rendering any {@link AbstractMessage} with built-in
 * scrolling and optional header controls. This is useful for displaying
 * messages in secondary UI locations like toolkit details or system instruction
 * previews.
 *
 * @author anahata
 */
public class OtherMessagePanel extends JPanel {

    private final ChatPanel chatPanel;
    private final AbstractMessage message;
    private final JScrollPane scrollPane;
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
        super(new BorderLayout());
        this.chatPanel = chatPanel;
        this.message = message;

        this.messagePanel = createMessagePanel(message);
        this.messagePanel.setRenderPruneButtons(renderPruneButtons);
        this.messagePanel.setRenderRemoveButtons(renderRemoveButtons);

        this.scrollPane = new JScrollPane(messagePanel);
        this.scrollPane.setBorder(null);
        this.scrollPane.setOpaque(false);
        this.scrollPane.getViewport().setOpaque(false);
        this.scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        this.scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        
        add(scrollPane, BorderLayout.CENTER);
        setOpaque(false);
    }

    private AbstractMessagePanel<AbstractMessage> createMessagePanel(AbstractMessage message) {
        return new AbstractMessagePanel<>(chatPanel, message) {
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
        };
    }
    
    /**
     * Triggers a re-render of the underlying message panel.
     */
    public void render() {
        messagePanel.render();
    }
}
