/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara!
 */
package uno.anahata.asi.agi.provider;

import lombok.Getter;

/**
 * A specialized exception indicating that an API call failed but is potentially retryable.
 * Providers can throw this exception to signal to the agi orchestrator that a retry
 * mechanism (like exponential backoff) should be engaged.
 */
@Getter
public class RetryableApiException extends RuntimeException {

    /** The API key used during the failed request invocation. */
    final String apiKey;

    /**
     * Constructs a new RetryableApiException with the specified API key, message, and cause.
     *
     * @param apiKey The API key used when the request was attempted.
     * @param message The detailed exception message.
     * @param cause The underlying cause of the request failure.
     */
    public RetryableApiException(String apiKey, String message, Throwable cause) {
        super(message, cause);
        this.apiKey = apiKey;
    }
    
    
}
