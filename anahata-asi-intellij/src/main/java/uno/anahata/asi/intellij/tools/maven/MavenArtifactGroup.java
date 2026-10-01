/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.maven;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Represents a consolidated Maven artifact with its coordinates, latest version, and all indexed versions.
 * <p>
 * Provides clean artifact-centric grouping, eliminating duplicate entries for historical versions
 * while supporting optional full version resolution.
 * </p>
 *
 * @author anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Represents a unique Maven artifact with its coordinates, latest version, and all indexed versions.")
public class MavenArtifactGroup {

    /** The Maven groupId. */
    @Schema(description = "The Maven groupId (e.g. 'org.junit.jupiter' or 'org.projectlombok').", example = "org.projectlombok")
    private String groupId;

    /** The Maven artifactId. */
    @Schema(description = "The Maven artifactId (e.g. 'lombok' or 'junit-jupiter-api').", example = "lombok")
    private String artifactId;

    /** The latest detected version of the artifact (or newest stable version if stableOnly was requested). */
    @Schema(description = "The latest detected version of the artifact.", example = "1.18.48")
    private String latestVersion;

    /** Total count of distinct versions available in the index for this artifact. */
    @Schema(description = "Total count of versions indexed for this artifact.", example = "16")
    private int totalVersionsCount;

    /** List of indexed versions sorted from newest to oldest. Null or empty if includeAllVersions was false. */
    @Schema(description = "List of indexed versions sorted newest-first. Populated if includeAllVersions was requested.")
    private List<String> versions;
}
