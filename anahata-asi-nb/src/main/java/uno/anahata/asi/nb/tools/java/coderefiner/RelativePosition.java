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
    @Schema(description = "Inserts the member at the absolute beginning of the target class container or file. (Ignores the 'anchorMemberName' parameter).")
    START, 
    
    /** Insert at the very end of the class or file. */
    @Schema(description = "Inserts the member at the absolute end of the target class container or file. (Ignores the 'anchorMemberName' parameter).")
    END, 
    
    /** Insert immediately before the specified anchor member. Anchor member becomes mandatory. */
    @Schema(description = "Inserts the member immediately BEFORE the existing member specified in 'anchorMemberName'. (Requires 'anchorMemberName' to be populated!).")
    BEFORE, 
    
    /** Insert immediately after the specified anchor member. Anchor member becomes mandatory. */
    @Schema(description = "Inserts the member immediately AFTER the existing member specified in 'anchorMemberName'. (Requires 'anchorMemberName' to be populated!).")
    AFTER
}
