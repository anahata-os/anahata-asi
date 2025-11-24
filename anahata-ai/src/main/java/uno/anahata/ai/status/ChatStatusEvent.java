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
package uno.anahata.ai.status;

import java.beans.PropertyChangeEvent;
import lombok.Getter;
import uno.anahata.ai.Chat;

/**
 * The base event object for all chat status changes.
 * It extends PropertyChangeEvent for compatibility with standard Java beans components.
 * 
 * @author pablo
 */
@Getter
public class ChatStatusEvent extends PropertyChangeEvent {
    
    private final Chat chat;
    
    private final ChatStatus status;
    
    public ChatStatusEvent(Chat source, ChatStatus status, String message) {
        super(source, status.name(), null, message);
        this.chat = source;
        this.status = status;
    }
    
    public String getMessage() {
        return (String) getNewValue();
    }
}
