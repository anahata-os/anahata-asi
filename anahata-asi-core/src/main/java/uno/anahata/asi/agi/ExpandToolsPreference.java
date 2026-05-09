/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi;

import java.io.Serializable;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * Defines the preference for the initial expansion state of tool calls in the UI.
 * 
 * @author anahata
 */
@Getter
@RequiredArgsConstructor
public enum ExpandToolsPreference implements Serializable {
    /**
     * All tool calls are initially expanded.
     */
    ALL("ALL"),
    /**
     * Only tool calls that require user confirmation (Permission is PROMPT) are initially expanded.
     */
    PROMPT("PROMPT"),
    /**
     * Tool calls are initially collapsed.
     */
    NONE("NONE");

    private final String displayValue;

    @Override
    public String toString() {
        return displayValue;
    }
}
