/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.maven;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * A data transfer object representing a single search result from the Maven index.
 * <p>
 * Contains fundamental coordinates and metadata for an artifact found across 
 * any of the configured repositories.
 * </p>
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Represents a single search result from the Maven index.")
public class MavenArtifactSearchResult {

    @Schema(description = "The groupId of the artifact.", example = "org.apache.commons")
    private String groupId;

    @Schema(description = "The artifactId of the artifact.", example = "commons-lang3")
    private String artifactId;

    @Schema(description = "The version of the artifact.", example = "3.12.0")
    private String version;

    @Schema(description = "The ID of the repository where the artifact was found.", example = "central")
    private String repositoryId;

    @Schema(description = "The packaging type of the artifact.", example = "jar")
    private String packaging;

    @Schema(description = "A brief description of the artifact if available.")
    private String description;
}