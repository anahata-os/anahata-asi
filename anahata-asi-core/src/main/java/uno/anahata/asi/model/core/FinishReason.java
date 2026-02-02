/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

/**
 * A standardized, model-agnostic enum representing the reason why a model
 * stopped generating content. This enum provides a 1:1 mapping for all 
 * known Gemini finish reasons, plus a specialized catch-all for unrecognized 
 * or missing reasons.
 *
 * @author anahata-gemini-pro-2.5
 */
public enum FinishReason {
    /** The finish reason was not provided by the model or is unspecified. */
    FINISH_REASON_UNSPECIFIED,
    /** Natural stop point of the model or provided stop sequence. */
    STOP,
    /** The maximum number of tokens as specified in the request was reached. */
    MAX_TOKENS,
    /** The content was flagged for safety reasons. */
    SAFETY,
    /** The content was flagged for recitation reasons (e.g., copyrighted material). */
    RECITATION,
    /** The content was flagged due to language constraints. */
    LANGUAGE,
    /** The reason is unknown or not specified by the provider. */
    OTHER,
    /** The content was blocked by a specific blocklist. */
    BLOCKLIST,
    /** The content contains prohibited material. */
    PROHIBITED_CONTENT,
    /** The content contains Sensitive Personally Identifiable Information (SPII). */
    SPII,
    /** The model produced a malformed function call. */
    MALFORMED_FUNCTION_CALL,
    /** The generated image failed safety checks. */
    IMAGE_SAFETY,
    /** The model made an unexpected tool call. */
    UNEXPECTED_TOOL_CALL,
    /** The generated image contains prohibited content. */
    IMAGE_PROHIBITED_CONTENT,
    /** No image was generated. */
    NO_IMAGE,
    /** The generated image contains copyrighted material. */
    IMAGE_RECITATION,
    /** The generated image was blocked for other reasons. */
    IMAGE_OTHER,
    /** Anahata-specific catch-all for reasons not recognized in this version. */
    GOD_FUCKING_KNOWS;
}
