/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.shell;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * Encapsulates the result of a shell command execution.
 * This POJO captures the process metadata, its lifecycle outcome (exit code), 
 * and the standard output generated during execution.
 * 
 * @author anahata
 */
@Schema(description = "Contains process details and exit code")
@Setter
@Getter
@ToString
public class ShellExecutionResult {
    
    /** 
     * The string representation of the underlying {@link java.lang.Process} object.
     */
    private String processToString;
    
    /** 
     * The operating system's process identifier (PID) for the executed command.
     */
    private String processId;
    
    /** 
     * The numeric exit code returned by the process upon completion. 
     * Conventionally, 0 indicates success.
     */
    private int exitCode;
    
    /** 
     * The full text captured from the process's standard output stream.
     */
    private String stdOut;
}
