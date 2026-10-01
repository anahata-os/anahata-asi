/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.maven;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Encapsulates the results of a Maven search across IntelliJ repository indexes.
 * <p>
 * Contains pagination metadata, the effective query or filter evaluated, and the list
 * of matching {@link MavenArtifactGroup} records.
 * </p>
 *
 * @author anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Search report containing matched Maven artifacts, pagination metadata, and index query details.")
public class MavenSearchReport {

    /** The search query or coordinate filter used for this search. */
    @Schema(description = "The search query or coordinate filter evaluated.", example = "lombok")
    private String query;

    /** The starting index (0-based) for this slice of results. */
    @Schema(description = "The starting index (0-based) for pagination.", example = "0")
    private int startIndex;

    /** Total count of distinct artifacts matching the search criteria. */
    @Schema(description = "Total count of distinct artifacts matching the search criteria.", example = "10")
    private int totalCount;

    /** The page of matching Maven artifact groups. */
    @Schema(description = "The list of matching Maven artifact groups for this page.")
    private List<MavenArtifactGroup> artifacts;
}
