package uno.anahata.ai.status;

/**
 * A listener for receiving chat status updates.
 *
 * @author pablo
 */
public interface StatusListener {

    /**
     * Called when the chat status changes.
     *
     * @param event The event object containing details about the status change.
     */
    void statusChanged(ChatStatusEvent event);
}
