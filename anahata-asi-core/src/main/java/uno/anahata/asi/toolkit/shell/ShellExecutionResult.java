/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package uno.anahata.asi.toolkit.shell;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 *
 * @author pablo
 */
@Schema(description = "Contains process details and exit code")
@Setter
@Getter
@ToString
public class ShellExecutionResult {
    private String processToString;
    private String processId;
    private int exitCode;
    private String stdOut;
    //private String stdErr;
}
