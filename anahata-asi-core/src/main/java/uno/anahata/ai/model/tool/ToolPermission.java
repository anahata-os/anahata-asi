/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.tool;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Represents the user's interactive, one-time choice for a specific tool invocation
 * within the UI prompt.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@AllArgsConstructor
public enum ToolPermission {
    /** Approve the invocation for this turn only. */
    PROMPT("Prompt"),
    /** Approve for this turn and save the preference as ALWAYS. */
    APPROVE_ALWAYS("Approve Always"),
    /** Deny for this turn and save the preference as NEVER. */
    DENY_NEVER("Deny Never");

    private final String displayValue;
}
