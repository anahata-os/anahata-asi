/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.tool;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Represents the lifecycle status of a single tool's execution.
 * This provides a more granular view than a simple success/failure flag.
 *
 * @author anahata-gemini-pro-2.5
 */
@Schema(description = "Execution status of a tool call")
public enum ToolExecutionStatus {
    /** The tool call has been created but not yet processed or executed. */
    @Schema(description = "user is reviewing your proposed tool call")
    PENDING,

    /** The tool was executed successfully. */
    @Schema(description = "tool call got executed once or multiple times")
    EXECUTED,
    
    /** The tool is current executing. */
    @Schema(description = "tool call is still going")
    EXECUTING,

    /** The tool execution was attempted but failed due to an exception. */
    @Schema(description = "tool call execution failed or failed pre-flight validation")
    FAILED,
    
    /** The tool execution was interrupted by the user. */
    @Schema(description = "tool call interrupted by the user or by something else")
    INTERRUPTED,

    /** The tool was not executed because it was not found in the list of enabled tools. */
    @Schema(description = "the name of the tool was a total hallucination")
    NOT_FOUND,

    /** 
     * The tool was not executed for a variety of reasons, such as being
     * declined by the user, disabled by preferences, or cancelled. The
     * specific reason should be in the 'error' field of the response.
     */
    @Schema(description = "she didnt like your tool call")
    DECLINED;
}
