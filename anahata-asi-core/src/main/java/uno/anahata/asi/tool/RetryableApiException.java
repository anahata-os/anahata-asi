/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara!
 */
package uno.anahata.asi.tool;

import lombok.Getter;

/**
 * A specialized exception indicating that an API call failed but is potentially retryable.
 * Providers can throw this exception to signal to the chat orchestrator that a retry
 * mechanism (like exponential backoff) should be engaged.
 */
@Getter
public class RetryableApiException extends RuntimeException {

    final String apiKey;

    public RetryableApiException(String apiKey, String message, Throwable cause) {
        super(message, cause);
        this.apiKey = apiKey;
    }
    
    
}
