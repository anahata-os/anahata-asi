/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.status;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * Defines the possible operational states of the Chat, primarily for UI feedback.
 * This is a direct port of the proven V1 enum.
 *
 * @author anahata
 */
@RequiredArgsConstructor
@Getter
public enum ChatStatus {
    /** The model has finished processing and is waiting for the user's next input. */
    IDLE("Idle, Waiting for User", "Waiting for user input.", false),

    /** A normal API call is in progress (e.g., waiting for a model response). */
    API_CALL_IN_PROGRESS("API Call in Progress...", "Waiting for a response from the model.", true),

    /** The assistant is waiting for the user to approve/deny tool calls. */
    TOOL_PROMPT("Tool Prompt", "Waiting for user to approve/deny tool calls.", true),

    /** The model has returned multiple candidates and is waiting for the user to choose one. */
    CANDIDATE_CHOICE_PROMPT("Candidate Choice", "Waiting for user to select a response candidate.", true),

    /** Local tool (function) execution is in progress. */
    TOOL_EXECUTION_IN_PROGRESS("Tool Execution...", "Executing local Java tools (functions).", true),

    /** An API error occurred, and the system is in retry mode with exponential backoff. */
    WAITING_WITH_BACKOFF("Waiting with Backoff...", "An API error occurred. Retrying with exponential backoff.", true),

    /** The assistant has hit the maximum number of retries and has stopped. */
    MAX_RETRIES_REACHED("Max Retries Reached", "The assistant has stopped after hitting the maximum number of retries.", false),
    
    /** A non-retryable API error occurred, or max retries were reached. */
    ERROR("Error", "An unrecoverable error occurred.", false),

    /** The chat session has been shut down. */
    SHUTDOWN("Shutdown", "The chat session has been shut down.", false);

    private final String displayName;
    private final String description;
    /**
     * Indicates if the chat is currently in an active state (e.g., processing, waiting for API, executing tools).
     * Non-active states include IDLE, ERROR, and SHUTDOWN.
     */
    private final boolean active;

    @Override
    public String toString() {
        return displayName;
    }
}
