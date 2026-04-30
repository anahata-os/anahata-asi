/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Enum.java to edit this template
 */
package uno.anahata.asi.nb.tools.java.coderefiner;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Defines positions for structural member insertion.
 */
public enum RelativePosition {
    @Schema(description = "Insert at the very beginning of the class or file.")
    START, @Schema(description = "Insert at the very end of the class or file.")
    END, @Schema(description = "Insert immediately before the specified anchor member.")
    BEFORE, @Schema(description = "Insert immediately after the specified anchor member.")
    AFTER

}
