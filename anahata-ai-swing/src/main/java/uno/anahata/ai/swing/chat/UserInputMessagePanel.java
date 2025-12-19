/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.Color;
import javax.swing.BorderFactory;
import javax.swing.border.Border;
import lombok.NonNull;
import uno.anahata.ai.model.core.InputUserMessage;
import uno.anahata.ai.swing.chat.render.AbstractMessagePanel;

/**
 * A panel that provides a live, read-only preview of the user's input message,
 * including markdown rendering of the text part and a display of any attached
 * blob parts.
 * <p>
 * This component is a standard JPanel that is updated by the parent InputPanel.
 *
 * @author pablo
 */
public class UserInputMessagePanel extends AbstractMessagePanel<InputUserMessage> {

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
}
