/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.tool;

/**
 * Represents the lifecycle status of a single tool's execution.
 * This provides a more granular view than a simple success/failure flag.
 *
 * @author anahata-gemini-pro-2.5
 */
public enum ToolExecutionStatus {
    /** The tool call has been created but not yet processed or executed. */
    PENDING,

    /** The tool was executed successfully. */
    EXECUTED,
    
    /** The tool is current executing. */
    EXECUTING,

    /** The tool execution was attempted but failed due to an exception. */
    FAILED,
    
    /** The tool execution was interrupted by the user. */
    INTERRUPTED,

    /** The tool was not executed because it was not found in the list of enabled tools. */
    NOT_FOUND,

    /** 
     * The tool was not executed for a variety of reasons, such as being
     * skipped by the user, disabled by preferences, or cancelled. The
     * specific reason should be in the 'error' field of the response.
     */
    NOT_EXECUTED;
}
