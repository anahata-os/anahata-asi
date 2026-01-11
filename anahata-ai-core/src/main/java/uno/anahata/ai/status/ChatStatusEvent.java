/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.status;

import java.beans.PropertyChangeEvent;
import lombok.Getter;
import uno.anahata.ai.chat.Chat;

/**
 * The base event object for all chat status changes.
 * It extends PropertyChangeEvent for compatibility with standard Java beans components.
 * 
 * @author anahata
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
