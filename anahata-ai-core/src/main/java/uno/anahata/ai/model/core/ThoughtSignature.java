/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara!
 */
package uno.anahata.ai.model.core;

/**
 * An interface for model-generated parts that can carry a thought signature.
 * This allows for a consistent way to access thought metadata across different
 * types of parts (e.g., text, tool calls, blobs).
 *
 * @author anahata-ai
 */
public interface ThoughtSignature {

    /**
     * Gets the signature of the thought process as a byte array.
     * @return The thought signature, or {@code null} if not present.
     */
    byte[] getThoughtSignature();

    /**
     * Sets the signature of the thought process as a byte array.
     * @param thoughtSignature The thought signature.
     */
    void setThoughtSignature(byte[] thoughtSignature);
}
