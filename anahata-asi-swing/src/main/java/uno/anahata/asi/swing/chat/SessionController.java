/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat;

import lombok.NonNull;
import uno.anahata.asi.chat.Chat;

/**
 * A controller interface for managing the lifecycle and focus of AI chat sessions
 * within the Swing UI. This interface decouples the session management logic
 * from the specific view implementation (e.g., table or cards).
 * 
 * @author anahata
 */
public interface SessionController {
    
    /**
     * Requests that the specified chat session be focused and brought to the front.
     * 
     * @param chat The chat session to focus.
     */
    void focus(@NonNull Chat chat);
    
    /**
     * Requests that the specified chat session's UI tab or window be closed.
     * This does not necessarily dispose of the session itself.
     * 
     * @param chat The chat session to close.
     */
    void close(@NonNull Chat chat);
    
    /**
     * Permanently disposes of the specified chat session, removing it from the
     * container and releasing its resources.
     * 
     * @param chat The chat session to dispose.
     */
    void dispose(@NonNull Chat chat);
    
    /**
     * Requests the creation of a new, empty AI chat session.
     */
    void createNew();
}
