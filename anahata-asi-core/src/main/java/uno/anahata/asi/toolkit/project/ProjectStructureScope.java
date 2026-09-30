/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.project;

import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serializable;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Configuration DTO defining the granularity scope for rendering project and module structures.
 * <p>
 * Controls what metadata attributes (Java element kinds, inner classes, VCS status badges,
 * physical file sizes, resources, root directory files, supertypes, and class Javadocs)
 * are included when the structure context provider generates Markdown for the AI prompt.
 * </p>
 * 
 * @author anahata
 */
@Schema(description = "Controls the granularity and detail level of project and module structure rendering.")
@Data
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
public class ProjectStructureScope implements Serializable {

    /** Whether to include compiler errors, project problems, and diagnostic alerts. */
    @Schema(description = "Whether to include compiler errors, project problems, and diagnostic alerts.", example = "true")
    @Builder.Default
    private boolean showAlerts = true;

    /** Whether to render Java element kinds (e.g. CLASS, INTERFACE, ENUM, RECORD). */
    @Schema(description = "Whether to render Java element kinds (e.g. CLASS, INTERFACE, ENUM, RECORD).", example = "true")
    @Builder.Default
    private boolean showElementKind = true;

    /** Whether to recursively render inner and nested classes beneath their enclosing type. */
    @Schema(description = "Whether to recursively render inner and nested classes.", example = "true")
    @Builder.Default
    private boolean showInnerClasses = true;

    /** Whether to append version control status badges (e.g. [M] for modified, [A] for added). */
    @Schema(description = "Whether to append version control status badges (e.g. [M], [A]).", example = "true")
    @Builder.Default
    private boolean showVcsStatus = true;

    /** Whether to append physical file sizes in human-readable format. */
    @Schema(description = "Whether to append physical file sizes in human-readable format.", example = "true")
    @Builder.Default
    private boolean showFileSizes = true;

    /** Whether to include non-Java resource directories (e.g. src/main/resources, .github). */
    @Schema(description = "Whether to include non-Java resource directories.", example = "true")
    @Builder.Default
    private boolean showResources = true;

    /** Whether to include top-level root files (e.g. pom.xml, README.md, anahata.md). */
    @Schema(description = "Whether to include top-level root files.", example = "true")
    @Builder.Default
    private boolean showRootFiles = true;

    /** Whether to list extended superclasses and implemented interfaces in a compact format. */
    @Schema(description = "Whether to list extended superclasses and implemented interfaces.", example = "false")
    @Builder.Default
    private boolean showSupertypes = false;

    /** Whether to include the first sentence of the class-level Javadoc summary. */
    @Schema(description = "Whether to include the first sentence of the class Javadoc summary.", example = "false")
    @Builder.Default
    private boolean showJavadoc = false;
}
