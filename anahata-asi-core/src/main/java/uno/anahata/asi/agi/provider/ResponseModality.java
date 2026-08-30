/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.provider;

import lombok.Getter;

/**
 * Represents the modalities that an AI model can output in its generative response.
 * <p>
 * This enumeration distinguishes between textual content and various generated binary
 * media types (images, audio streams, and video).
 * </p>
 * 
 * @author anahata
 */
@Getter
public enum ResponseModality {

    /** 
     * Textual output, markdown, code, reasoning thoughts, and function/tool calls. 
     */
    TEXT("Text"),

    /** 
     * Binary image generation and visual editing artifacts. 
     */
    IMAGE("Image"),

    /** 
     * Binary audio synthesis, voice streams, speech-to-speech, and music generation. 
     */
    AUDIO("Audio"),

    /** 
     * Video generation and video synthesis streams. 
     */
    VIDEO("Video");

    /**
     * The human-readable display name for this response modality.
     */
    private final String displayName;

    /**
     * Constructs a ResponseModality enum constant with its display name.
     *
     * @param displayName The human-readable display name.
     */
    ResponseModality(String displayName) {
        this.displayName = displayName;
    }

    /**
     * {@inheritDoc}
     * <p>Returns the human-readable display name.</p>
     */
    @Override
    public String toString() {
        return displayName;
    }

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
