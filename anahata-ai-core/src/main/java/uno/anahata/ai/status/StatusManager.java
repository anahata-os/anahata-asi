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

/**
 * Manages and broadcasts the real-time status of the chat session.
 *
 * @author anahata
 */
@Slf4j
@Getter
public class StatusManager {

    private final Chat chat;
    private final List<StatusListener> listeners = new ArrayList<>();
    private final List<ApiErrorRecord> apiErrors = new ArrayList<>();

    private ChatStatus currentStatus = ChatStatus.IDLE; // Corrected to IDLE
    private String executingToolName;
    private long statusChangeTime = System.currentTimeMillis();
    private long lastOperationDuration;
    private long currentBackoffAmount; // New field for backoff amount

    public StatusManager(@NonNull Chat chat) {
        this.chat = chat;
    }

    public void addListener(StatusListener listener) {
        listeners.add(listener);
    }

    public void removeListener(StatusListener listener) {
        listeners.remove(listener);
    }

    /**
     * Fires a status change event to all listeners.
     *
     * @param newStatus The new status.
     */
    public void fireStatusChanged(ChatStatus newStatus) {
        fireStatusChanged(newStatus, null);
    }

    /**
     * Fires a status change event to all listeners, optionally with a detail message.
     *
     * @param newStatus The new status.
     * @param detailMessage A detail message (e.g., for tool execution).
     */
    public void fireStatusChanged(ChatStatus newStatus, String detailMessage) {
        if (this.currentStatus != newStatus) {
            log.info("Status changed from {} to {}", this.currentStatus, newStatus);
            this.currentStatus = newStatus;
            this.statusChangeTime = System.currentTimeMillis();
        }

        if (newStatus == ChatStatus.TOOL_EXECUTION_IN_PROGRESS) {
            this.executingToolName = detailMessage;
        } else {
            this.executingToolName = null;
        }

        if (newStatus == ChatStatus.IDLE) { // Corrected to IDLE
            this.lastOperationDuration = System.currentTimeMillis() - statusChangeTime;
        } else {
            this.lastOperationDuration = 0;
        }

        fireEvent(new ChatStatusEvent(chat, newStatus, detailMessage));
    }

    /**
     * Fires the given event to all registered listeners.
     *
     * @param event The event to fire.
     */
    private void fireEvent(ChatStatusEvent event) {
        for (StatusListener listener : listeners) {
            listener.statusChanged(event);
        }
    }

    /**
     * Records an API error and sets the chat status.
     * This method is designed to be called from the Chat orchestrator to centralize
     * error reporting and status updates.
     *
     * @param errorRecord The ApiErrorRecord to record.
     * @param status The new chat status to set.
     * @param detailMessage A detail message for the status change.
     */
    public void fireApiError(ApiErrorRecord errorRecord, ChatStatus status, String detailMessage) {
        apiErrors.add(errorRecord);
        this.currentBackoffAmount = errorRecord.getBackoffAmount(); // Store backoff amount
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
        this.currentBackoffAmount = 0; // Reset backoff amount on clear
    }

    /**
     * Resets the status manager to its initial state.
     */
    public void reset() {
        this.currentStatus = ChatStatus.IDLE; // Corrected to IDLE
        this.executingToolName = null;
        this.statusChangeTime = System.currentTimeMillis();
        this.lastOperationDuration = 0;
        this.apiErrors.clear();
        this.currentBackoffAmount = 0; // Reset backoff amount on reset
    }
}
