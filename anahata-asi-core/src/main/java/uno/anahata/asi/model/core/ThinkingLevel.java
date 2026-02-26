/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * Standardized, model-agnostic enum representing the level of "thinking" or 
 * reasoning tokens that the model should generate.
 * 
 * @author anahata-ai
 */
@Getter
@RequiredArgsConstructor
public enum ThinkingLevel {
    /** Unspecified thinking level. */
    THINKING_LEVEL_UNSPECIFIED("Unspecified"),
    /** Low thinking level. */
    LOW("Low"),
    /** Medium thinking level. */
    MEDIUM("Medium"),
    /** High thinking level. */
    HIGH("High"),
    /** MINIMAL thinking level. */
    MINIMAL("Minimal");

    /** The human-readable display value. */
    private final String displayValue;
}
