/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.maven;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
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

    /** The groupId of the artifact (e.g., 'org.apache.commons'). */
    @Schema(description = "The groupId of the artifact.", example = "org.apache.commons")
    private String groupId;

    /** The artifactId of the artifact (e.g., 'commons-lang3'). */
    @Schema(description = "The artifactId of the artifact.", example = "commons-lang3")
    private String artifactId;

    /** The version of the artifact (e.g., '3.12.0'). */
    @Schema(description = "The version of the artifact.", example = "3.12.0")
    private String version;

    /** The ID of the repository where the artifact was found (e.g., 'central'). */
    @Schema(description = "The ID of the repository where the artifact was found.", example = "central")
    private String repositoryId;

    /** The packaging type of the artifact (e.g., 'jar', 'nbm'). */
    @Schema(description = "The packaging type of the artifact.", example = "jar")
    private String packaging;

    /** A brief description of the artifact if provided by the Maven index. */
    @Schema(description = "A brief description of the artifact if available.")
    private String description;

    /** Available classifiers for this artifact version (e.g. ['sources', 'javadoc']). */
    @Schema(description = "Available classifiers for this artifact version (e.g. ['sources', 'javadoc', 'natives-linux']).")
    private List<String> availableClassifiers;

    /** Indicates whether this result represents the latest detected version of the artifact. */
    @Schema(description = "Whether this result represents the latest detected version of the artifact.")
    private Boolean latest;

    /**
     * Constructs an artifact search result with core coordinates and metadata.
     * 
     * @param groupId The groupId of the artifact.
     * @param artifactId The artifactId of the artifact.
     * @param version The version of the artifact.
     * @param repositoryId The ID of the repository.
     * @param packaging The packaging type.
     * @param description A brief description.
     */
    public MavenArtifactSearchResult(String groupId, String artifactId, String version, String repositoryId, String packaging, String description) {
        this.groupId = groupId;
        this.artifactId = artifactId;
        this.version = version;
        this.repositoryId = repositoryId;
        this.packaging = packaging;
        this.description = description;
    }
}