/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.status;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.core.BasicPropertyChangeSource;

/**
 * Manages and broadcasts the real-time status of the chat session.
 * This class leverages PropertyChangeSupport to provide reactive updates
 * to the UI.
 *
 * @author anahata
 */
@Slf4j
@Getter
public class StatusManager extends BasicPropertyChangeSource {

    private final Chat chat;
    private final List<ApiErrorRecord> apiErrors = new ArrayList<>();

    private ChatStatus currentStatus = ChatStatus.IDLE; 
    private long statusChangeTime = System.currentTimeMillis();
    private long lastOperationDuration;
    private long currentBackoffAmount; 

    public StatusManager(@NonNull Chat chat) {
        this.chat = chat;
    }

    /**
     * Fires a status change event.
     *
     * @param newStatus The new status.
     */
    public void fireStatusChanged(ChatStatus newStatus) {
        fireStatusChanged(newStatus, null);
    }

    /**
     * Fires a status change event, optionally with a detail message.
     *
     * @param newStatus The new status.
     * @param detailMessage A detail message (e.g., for tool execution).
     */
    public void fireStatusChanged(ChatStatus newStatus, String detailMessage) {
        ChatStatus oldStatus = this.currentStatus;

        if (this.currentStatus != newStatus) {
            log.info("Status changed from {} to {}", this.currentStatus, newStatus);
            this.currentStatus = newStatus;
            this.statusChangeTime = System.currentTimeMillis();
        }

        if (newStatus == ChatStatus.IDLE) { 
            this.lastOperationDuration = System.currentTimeMillis() - statusChangeTime;
        } else {
            this.lastOperationDuration = 0;
        }

        // Notify PropertyChangeListeners (Reactive UI)
        getPropertyChangeSupport().firePropertyChange("currentStatus", oldStatus, newStatus);
    }

    /**
     * Records an API error and sets the chat status.
     *
     * @param errorRecord The ApiErrorRecord to record.
     * @param status The new chat status to set.
     * @param detailMessage A detail message for the status change.
     */
    public void fireApiError(ApiErrorRecord errorRecord, ChatStatus status, String detailMessage) {
        apiErrors.add(errorRecord);
        this.currentBackoffAmount = errorRecord.getBackoffAmount(); 
        fireStatusChanged(status, detailMessage);
    }

    /**
     * Gets an unmodifiable list of all recorded API errors.
     *
     * @return The list of errors.
     */
    public List<ApiErrorRecord> getApiErrors() {
        return Collections.unmodifiableList(apiErrors);
    }
    
    /**
     * Clears all recorded API errors. This should be called upon a successful API response.
     */
    public void clearApiErrors() {
        apiErrors.clear();
        this.currentBackoffAmount = 0; 
    }

    /**
     * Resets the status manager to its initial state.
     */
    public void reset() {
        this.currentStatus = ChatStatus.IDLE; 
        this.statusChangeTime = System.currentTimeMillis();
        this.lastOperationDuration = 0;
        this.apiErrors.clear();
        this.currentBackoffAmount = 0; 
    }
}
