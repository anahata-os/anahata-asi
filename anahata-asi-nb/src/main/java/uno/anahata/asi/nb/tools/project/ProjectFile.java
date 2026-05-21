/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.project;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Represents a single file within a project, including its metadata and status.
 * <p>
 * This DTO is used to provide the ASI with a detailed view of project resources, 
 * including their size, last modification time, and absolute physical path.
 * </p>
 *
 * @author anahata
 */
@Schema(description = "Represents a single file within a project, including its metadata and its status in the conversation context.")
@Data
@AllArgsConstructor
public final class ProjectFile {

    /** The simple name of the file (e.g., 'pom.xml'). */
    @Schema(description = "The name of the file.", example = "pom.xml")
    private final String name;

    /** The name of the file including IDE annotations (e.g. Git status badges). */
    @Schema(description = "The name of the file including IDE annotations (e.g. Git status).")
    private final String annotatedName;

    /** The size of the file in bytes. */
    @Schema(description = "The size of the file in bytes.", example = "4096")
    private final long size;

    /** The last modified timestamp of the file on disk (Epoch millis). */
    @Schema(description = "The last modified timestamp of the file on disk.", example = "1711462000000")
    private final long lastModified;
    
    /** The absolute physical path to the file. */
    @Schema(description = "The absolute path to the file.", example = "/home/pablo/project/pom.xml")
    private final String path;
}