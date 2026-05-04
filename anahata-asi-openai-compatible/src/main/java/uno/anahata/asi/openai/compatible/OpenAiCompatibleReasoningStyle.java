/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai.compatible;

/**
 * Defines the strategy for extracting reasoning/thought content from an 
 * OpenAI-compatible response.
 */
public enum OpenAiCompatibleReasoningStyle {
    /** No specialized reasoning extraction. All content is treated as final output. */
    NONE,
    /** Reasoning is provided in a dedicated top-level field (e.g., 'reasoning_content'). */
    FIELD,
    /** Reasoning is embedded within the main content field wrapped in specific tags (e.g., <think>...</think>). */
    TAGS
}
