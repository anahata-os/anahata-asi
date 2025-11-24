/*
 * Copyright 2025 Anahata.
 *
 * Licensed under the Anahata Software License (ASL) V2.0;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://github.com/pablo-anahata/anahata-ai-parent/blob/main/LICENSE
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Fora Bara!
 */
package uno.anahata.ai.context.provider;

import java.util.Collections;
import java.util.List;
import uno.anahata.ai.Chat;
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
