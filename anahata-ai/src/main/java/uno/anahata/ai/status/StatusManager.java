/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.status;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import uno.anahata.ai.Chat;

/**
 * Manages and broadcasts chat status events to registered listeners.
 * This is a direct port of the proven V1 StatusManager, using the standard
 * java.beans.PropertyChangeSupport for robust, decoupled event handling.
 * 
 * @author pablo
 */
@RequiredArgsConstructor
public class StatusManager {

    private final PropertyChangeSupport changeSupport = new PropertyChangeSupport(this);
    
    @Getter
    private final Chat chat;

    @Getter
    private ChatStatusEvent lastEvent;

    public void fireStatusChanged(ChatStatus status) {
        fireStatusChanged(status, status.getDescription());
    }

    public void fireStatusChanged(ChatStatus status, String message) {
        lastEvent = new ChatStatusEvent(chat, status, message);
        changeSupport.firePropertyChange(lastEvent);
    }

    public void addPropertyChangeListener(PropertyChangeListener listener) {
        changeSupport.addPropertyChangeListener(listener);
    }

    public void removePropertyChangeListener(PropertyChangeListener listener) {
        changeSupport.removePropertyChangeListener(listener);
    }
}
