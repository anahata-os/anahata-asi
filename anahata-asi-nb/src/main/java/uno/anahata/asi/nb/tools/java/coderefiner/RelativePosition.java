/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Defines positions for structural member insertion.
 * 
 * @author anahata
 */
public enum RelativePosition {
    /** Insert at the very beginning of the class or file. */
    @Schema(description = "Insert at the very beginning of the class or file.")
    START, 
    
    /** Insert at the very end of the class or file. */
    @Schema(description = "Insert at the very end of the class or file.")
    END, 
    
    /** Insert immediately before the specified anchor member. Anchor member becomes mandatory. */
    @Schema(description = "Insert immediately before the specified anchor member. Anchor member becomes mandatory")
    BEFORE, 
    
    /** Insert immediately after the specified anchor member. Anchor member becomes mandatory. */
    @Schema(description = "Insert immediately after the specified anchor member. Anchor member becomes mandatory")
    AFTER
}
