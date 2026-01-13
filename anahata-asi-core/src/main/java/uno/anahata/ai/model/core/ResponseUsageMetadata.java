/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.model.core;

import lombok.Value;
import lombok.experimental.SuperBuilder;

/**
 * Metadata about the token usage for a single API response.
 *
 * @author anahata
 */
@Value
@SuperBuilder
public class ResponseUsageMetadata {

    /**
     * The number of tokens in the prompt.
     */
    int promptTokenCount;

    /**
     * The number of tokens in the generated candidates.
     */
    int candidatesTokenCount;

    /**
     * The number of tokens in the cached content.
     */
    int cachedContentTokenCount;

    /**
     * The number of tokens used for model thoughts (internal processing).
     */
    int thoughtsTokenCount;

    /**
     * The number of tokens in the results from tool executions, which are provided back
     * to the model as input, if applicable.
     */
    int toolUsePromptTokenCount;

    /**
     * The total number of tokens used for the entire interaction.
     */
    int totalTokenCount;

    /**
     * The raw JSON representation of the native usage metadata object.
     */
    String rawJson;
}
