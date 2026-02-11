/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Represents a single file within a project, including its metadata and status.
 * This is a Java 8-compatible, immutable data class.
 *
 * @author Anahata
 */
@Schema(description = "Represents a single file within a project, including its metadata and its status in the conversation context.")
@Data
@AllArgsConstructor
public final class ProjectFile {

    @Schema(description = "The name of the file.")
    private final String name;

    @Schema(description = "The name of the file including IDE annotations (e.g. Git status).")
    private final String annotatedName;

    @Schema(description = "The size of the file in bytes.")
    private final long size;

    @Schema(description = "The last modified timestamp of the file on disk.")
    private final long lastModified;
    
    @Schema(description = "The absolute path to the file.")
    private final String path;
}