/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.context.provider;

import java.util.Collections;
import java.util.List;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.status.ChatStatus;

/**
 * A context provider that injects the current status of the chat into the prompt.
 * This is a direct port of the proven V1 provider.
 * 
 * @author pablo
 */
public class ChatStatusProvider extends AbstractContextProvider {

    public ChatStatusProvider() {
        super("core-chat-status", "Chat Status", "Provides the current status of the chat session.", ContextPosition.PROMPT_AUGMENTATION);
    }

    @Override
    public List<AbstractPart> getParts(Chat chat) throws Exception {
        ChatStatus status = chat.getStatusManager().getLastEvent() != null
            ? chat.getStatusManager().getLastEvent().getStatus()
            : ChatStatus.IDLE_WAITING_FOR_USER;
            
        String statusString = "- Chat Status: " + status.getDisplayName() + "\n";
        return Collections.singletonList(new TextPart(statusString));
    }
}
