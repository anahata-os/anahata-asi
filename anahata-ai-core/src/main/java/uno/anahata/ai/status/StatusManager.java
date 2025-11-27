/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.status;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.chat.Chat;

/**
 * Manages and broadcasts chat status events to registered listeners.
 * This is a direct port of the proven V1 StatusManager, using a type-safe listener pattern.
 *
 * @author pablo
 */
@RequiredArgsConstructor
@Slf4j
public class StatusManager {

    private final List<StatusListener> listeners = new CopyOnWriteArrayList<>();

    @Getter
    private final Chat chat;

    @Getter
    private ChatStatusEvent lastEvent;

    public void fireStatusChanged(ChatStatus status) {
        fireStatusChanged(status, status.getDescription());
    }

    public void fireStatusChanged(ChatStatus status, String message) {
        lastEvent = new ChatStatusEvent(chat, status, message);
        for (StatusListener listener : listeners) {
            try {
                listener.statusChanged(lastEvent);
            } catch (Exception e) {
                log.error("StatusListener {} threw an exception", listener.getClass().getName(), e);
            }
        }
    }

    public void addStatusListener(StatusListener listener) {
        listeners.add(listener);
    }

    public void removeStatusListener(StatusListener listener) {
        listeners.remove(listener);
    }
}
