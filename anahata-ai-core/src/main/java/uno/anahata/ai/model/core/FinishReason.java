/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.core;

/**
 * A standardized, model-agnostic enum representing the reason why a model
 * stopped generating content.
 *
 * @author anahata-gemini-pro-2.5
 */
public enum FinishReason {
    /** Natural stop point of the model or provided stop sequence. */
    STOP,
    /** The maximum number of tokens as specified in the request was reached. */
    MAX_TOKENS,
    /** The content was flagged for safety reasons. */
    SAFETY,
    /** The content was flagged for recitation reasons. */
    RECITATION,
    /** The model decided to call a tool. */
    TOOL_EXECUTION,
    /** The reason is unknown or not specified. */
    OTHER;
}
