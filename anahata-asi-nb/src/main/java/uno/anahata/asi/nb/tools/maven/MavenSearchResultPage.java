/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.maven;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Represents a paginated result set from a Maven index search.
 * <p>
 * This DTO encapsulates a subset of results along with pagination metadata 
 * to allow for efficient browsing of large search indexes.
 * </p>
 * 
 * @author anahata
 */
@Data
@AllArgsConstructor
@Schema(description = "Represents a paginated result set from a Maven index search.")
public class MavenSearchResultPage {
    
    @Schema(description = "The starting index (0-based) for pagination.")
    private final int startIndex;

    @Schema(description = "The total number of results found matching the query.")
    private final int totalCount;

    @Schema(description = "The list of artifacts for the current page.")
    private final List<MavenArtifactSearchResult> page;
}