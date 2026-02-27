/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi;

import lombok.NonNull;
import uno.anahata.asi.agi.Agi;

/**
 * A controller interface for managing the lifecycle and focus of AI agi sessions
 * within the Swing UI. This interface decouples the session management logic
 * from the specific view implementation (e.g., table or cards).
 * 
 * @author anahata
 */
public interface SessionController {
    
    /**
     * Requests that the specified agi session be focused and brought to the front.
     * 
     * @param agi The agi session to focus.
     */
    void focus(@NonNull Agi agi);
    
    /**
     * Requests that the specified agi session's UI tab or window be closed.
     * This does not necessarily dispose of the session itself.
     * 
     * @param agi The agi session to close.
     */
    void close(@NonNull Agi agi);
    
    /**
     * Permanently disposes of the specified agi session, removing it from the
     * container and releasing its resources.
     * 
     * @param agi The agi session to dispose.
     */
    void dispose(@NonNull Agi agi);
    
    /**
     * Requests the creation of a new, empty AI agi session.
     */
    void createNew();
}
