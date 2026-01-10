/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.status;

import java.util.Collections;
import java.util.List;
import lombok.RequiredArgsConstructor;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.context.AbstractContextProvider;
import uno.anahata.ai.model.core.AbstractModelMessage;
import uno.anahata.ai.model.core.RagMessage;

/**
 * Provides real-time status information about the chat session to the model.
 *
 * @author anahata-ai
 */
public class ChatStatusProvider extends AbstractContextProvider {

    private final Chat chat;

    public ChatStatusProvider(Chat chat) {
        super("chat-status", "Chat Status", "Provides real-time status information about the chat session.");
        this.chat = chat;
    }

    @Override
    public List<String> getSystemInstructions(Chat chat) throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append("## Chat Status\n");
        sb.append("- Session ID: ").append(chat.getConfig().getSessionId()).append("\n");
        sb.append("- Model ID: ").append(chat.getSelectedModel() != null ? chat.getSelectedModel().getModelId() : "None").append("\n");
        sb.append("- Status: ").append(chat.getStatusManager().getCurrentStatus()).append("\n");
        sb.append("- Running: ").append(chat.isRunning()).append("\n");
        
        AbstractModelMessage toolPromptMessage = chat.getToolPromptMessage();
        if (toolPromptMessage != null) {
            sb.append("- Tool Prompt Message ID: ").append(toolPromptMessage.getSequentialId()).append("\n");
        }
        
        return Collections.singletonList(sb.toString());
    }

    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        // No prompt augmentation needed for chat status at this time.
    }
}
