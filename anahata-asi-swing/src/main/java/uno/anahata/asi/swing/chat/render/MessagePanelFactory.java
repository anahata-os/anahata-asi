/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.render;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.UserMessage;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * A factory for creating {@link AbstractMessagePanel} instances based on the 
 * concrete type of the {@link AbstractMessage}.
 * <p>
 * This factory centralizes the mapping between message domain objects and their 
 * corresponding UI components, ensuring consistency across the application.
 * </p>
 *
 * @author anahata
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class MessagePanelFactory {

    /**
     * Creates the appropriate message panel for the given message.
     * 
     * @param chatPanel The parent chat panel.
     * @param message The message to render.
     * @return The created message panel, or null if the message type is unsupported.
     */
    public static AbstractMessagePanel<?> createMessagePanel(ChatPanel chatPanel, AbstractMessage message) {
        if (message instanceof UserMessage userMessage) {
            return new UserMessagePanel(chatPanel, userMessage);
        } else if (message instanceof AbstractModelMessage modelMessage) {
            return new ModelMessagePanel(chatPanel, modelMessage);
        }
        return null;
    }
}
