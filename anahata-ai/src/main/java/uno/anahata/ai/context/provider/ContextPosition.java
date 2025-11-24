/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.context.provider;

/**
 * Defines the position where a ContextProvider's content should be injected into the request.
 * This is a direct port of the proven V1 enum.
 * 
 * @author pablo
 */
public enum ContextPosition {
    /**
     * The content should be treated as system instructions, which are typically
     * prepended to the conversation or sent as a separate parameter to the API.
     * Most models only support TextParts in this position.
     */
    SYSTEM_INSTRUCTIONS,
    
    /**
     * The content should be appended to the end of the user's prompt as a synthetic
     * UserMessage, providing just-in-time context or workspace information.
     * This position supports both TextParts and BlobParts.
     */
    PROMPT_AUGMENTATION;
}
