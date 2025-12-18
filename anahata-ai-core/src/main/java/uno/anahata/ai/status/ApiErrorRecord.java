/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.status;

import java.time.Instant;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

/**
 * A record of a single API error, including context for retries.
 *
 * @author anahata
 */
@Getter
@SuperBuilder
public class ApiErrorRecord {

    /**
     * The ID of the model that was being called.
     */
    String modelId;

    /**
     * The timestamp when the error occurred.
     */
    Instant timestamp;

    /**
     * The attempt number when the error occurred (0-based).
     */
    int retryAttempt;

    /**
     * The backoff amount in milliseconds before the next retry.
     */
    @Setter
    long backoffAmount;

    /**
     * The exception that was thrown.
     */
    Throwable exception;

    /**
     * The API key used when the error occurred (abbreviated).
     */
    @Setter
    String apiKey;
}
