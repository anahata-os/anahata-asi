/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.Color;
import javax.swing.BorderFactory;
import javax.swing.border.Border;
import lombok.NonNull;
import uno.anahata.ai.model.core.UserMessage;
import uno.anahata.ai.swing.chat.ChatPanel;

/**
 * A concrete implementation of {@link AbstractMessagePanel} specifically for rendering
 * {@link UserMessage} instances in the conversation history.
 *
 * @author pablo
 */
public class UserMessagePanel extends AbstractMessagePanel<UserMessage> {

    /**
     * Constructs a new UserMessagePanel.
     *
     * @param chatPanel The parent chat panel.
     * @param message The user message to render.
     */
    public UserMessagePanel(@NonNull ChatPanel chatPanel, @NonNull UserMessage message) {
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
}
