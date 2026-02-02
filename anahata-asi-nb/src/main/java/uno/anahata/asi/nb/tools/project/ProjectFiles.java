/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Represents the file and folder structure of a project.
 */
@Schema(description = "Represents the file and folder structure of a project, including root files and a detailed source tree.")
@Data
@AllArgsConstructor
public final class ProjectFiles {

    @Schema(description = "A list of files located directly in the project's root directory.")
    private final List<ProjectFile> rootFiles;

    @Schema(description = "A list of the names of all folders located in the project's root directory.")
    private final List<String> rootFolderNames;

    @Schema(description = "A detailed, recursive tree structure of the project's primary source code folders.")
    private final List<SourceFolder> sourceFolders;
}
