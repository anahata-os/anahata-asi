/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.ide;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Represents detailed metadata about an open TopComponent (window) in the IDE.
 * <p>
 * This DTO aggregates information from the NetBeans windowing system, 
 * including identification, visual state, project context, and associated files.
 * </p>
 */
@Schema(description = "Represents detailed information about an open TopComponent (window) in the IDE.")
@Data
@AllArgsConstructor
public final class TopComponentInfo {
    /** The unique identifier of the TopComponent window. */
    private final String id;
    /** The basic name of the window. */
    private final String name;
    /** Whether this window is currently selected and active. */
    private final boolean selected;
    /** The user-visible display name of the window. */
    private final String displayName;
    /** The HTML-enhanced display name of the window (if configured). */
    private final String htmlDisplayName;
    /** The tooltip description text associated with the window. */
    private final String tooltip;
    /** The fully qualified class name of this TopComponent implementation. */
    private final String className;
    /** The display mode (e.g. editor, explorer, output) where the window resides. */
    private final String mode;
    /** Summarized list of nodes currently selected/activated within this window. */
    private final String activatedNodes;
    /** Actions supported by this window as a comma-separated list. */
    private final String supportedActions;
    /** The absolute path of the primary file represented by this window (if any). */
    private final String filePath;
    /** The primary file path representation for multi-view or grouped editors. */
    private final String primaryFilePath;
    /** The size in bytes of the file represented by this window. */
    private final long sizeInBytes;
}
