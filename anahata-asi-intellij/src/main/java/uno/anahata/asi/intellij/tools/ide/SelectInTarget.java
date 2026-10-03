/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.ide;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Defines the supported target views for programmatic selection in the IntelliJ IDEA IDE.
 *
 * @author anahata
 */
public enum SelectInTarget {

    /**
     * The logical Project view in the Project tool window (Alt+1).
     */
    @Schema(description = "The logical Project view in the Project tool window (Alt+1).")
    PROJECTS,

    /**
     * The File Structure tool window (Alt+7).
     */
    @Schema(description = "The File Structure tool window (Alt+7).")
    STRUCTURE,

    /**
     * The physical file manager in the host operating system (Show in Files / Explorer / Finder).
     */
    @Schema(description = "The physical file manager in the host operating system (Show in Files / Explorer / Finder).")
    FILES
}
