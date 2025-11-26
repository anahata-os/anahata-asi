/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.tool;

/**
 * Represents the user's interactive, one-time choice for a specific tool invocation
 * within the UI prompt.
 *
 * @author anahata-gemini-pro-2.5
 */
public enum ToolPermission {
    /** Approve the invocation for this turn only. */
    APPROVE,
    /** Deny the invocation for this turn only. */
    DENY,
    /** Approve for this turn and save the preference as ALWAYS. */
    APPROVE_ALWAYS,
    /** Deny for this turn and save the preference as NEVER. */
    DENY_NEVER;
}
