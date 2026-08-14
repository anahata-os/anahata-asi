/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Defines the NetBeans formatting mode for BatchCodeRefiner operations.
 * 
 * @author anahata
 */
public enum FormatMode {
    /** Do not apply NetBeans Reformat. */
    @Schema(description = "Do not apply NetBeans Reformat.")
    NONE,

    /** Formats only the specific character ranges of inserted or updated members (Default). */
    @Schema(description = "Formats only the specific character ranges of inserted or updated members (Default).")
    SELECTED_RANGES,

    /** Formats the entire document after applying changes. */
    @Schema(description = "Formats the entire document after applying changes.")
    ENTIRE_DOCUMENT
}
