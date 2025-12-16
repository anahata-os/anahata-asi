/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara!
 */
package uno.anahata.ai.tool;

/**
 * A specialized exception indicating that an API call failed but is potentially retryable.
 * Providers can throw this exception to signal to the chat orchestrator that a retry
 * mechanism (like exponential backoff) should be engaged.
 */
public class RetryableApiException extends RuntimeException {

    public RetryableApiException(String message) {
        super(message);
    }

    public RetryableApiException(String message, Throwable cause) {
        super(message, cause);
    }
}
