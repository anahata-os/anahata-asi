/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.provider;

/**
 * Represents the modalities that an AI model can output in its generative response.
 * <p>
 * This enumeration distinguishes between textual content and various generated binary
 * media types (images, audio streams, and video).
 * </p>
 * 
 * @author anahata
 */
public enum ResponseModality {

    /** 
     * Textual output, markdown, code, reasoning thoughts, and function/tool calls. 
     */
    TEXT,

    /** 
     * Binary image generation and visual editing artifacts. 
     */
    IMAGE,

    /** 
     * Binary audio synthesis, voice streams, speech-to-speech, and music generation. 
     */
    AUDIO,

    /** 
     * Video generation and video synthesis streams. 
     */
    VIDEO;

    /**
     * Parses a string into a {@link ResponseModality} case-insensitively.
     *
     * @param name The name or representation of the modality.
     * @return The matching {@link ResponseModality}, or {@code null} if unmatched or null.
     */
    public static ResponseModality parse(String name) {
        if (name == null || name.isBlank()) {
            return null;
        }
        for (ResponseModality m : values()) {
            if (m.name().equalsIgnoreCase(name.trim())) {
                return m;
            }
        }
        return null;
    }
}
