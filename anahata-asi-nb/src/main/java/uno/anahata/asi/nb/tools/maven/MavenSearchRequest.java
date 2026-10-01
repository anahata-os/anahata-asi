/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.maven;

import uno.anahata.asi.nb.tools.maven.MavenQueryClause;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Encapsulates search parameters for unified Maven repository index querying.
 * <p>
 * Supports both unstructured keyword queries (which are automatically split and joined with AND semantics)
 * and fully structured {@link MavenQueryClause} boolean query trees. Also provides sorting, version collapsing,
 * and stable-only filtering options.
 * </p>
 * 
 * @author anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Comprehensive search request for querying configured Maven repository indexes.")
public class MavenSearchRequest {

    /**
     * Strategy for sorting search results.
     */
    @Schema(description = "Strategy for ordering search results.")
    public enum SortStrategy {
        /** Sorts by newest version first using Maven ComparableVersion comparison. */
        @Schema(description = "Sorts by newest version first using Maven ComparableVersion comparison.")
        LATEST_VERSION,

        /** Sorts by Lucene relevance score. */
        @Schema(description = "Sorts by Lucene relevance score.")
        RELEVANCE,

        /** Sorts alphabetically by groupId, then artifactId, then version. */
        @Schema(description = "Sorts alphabetically by groupId, then artifactId, then version.")
        COORDINATES
    }

    /** Structured query clause tree. If provided, takes precedence over raw query string. */
    @Schema(description = "Structured boolean query clause tree (AND/OR/NOT groups, field targeting). Takes precedence over 'query'.")
    private MavenQueryClause clause;

    /** Simple keyword query string (e.g. 'junit platform' or 'spring security'). */
    @Schema(description = "Simple text query string (e.g. 'junit platform' or 'guava'). Split by spaces and evaluated with AND logic.", example = "junit platform")
    private String query;

    /** Exact groupId filter shortcut. */
    @Schema(description = "Exact groupId filter shortcut (e.g. 'org.junit.jupiter').", example = "org.junit.jupiter")
    private String groupId;

    /** Exact artifactId filter shortcut. */
    @Schema(description = "Exact artifactId filter shortcut (e.g. 'junit-jupiter-api').", example = "junit-jupiter-api")
    private String artifactId;

    /** Strategy for sorting search results. Defaults to LATEST_VERSION. */
    @Schema(description = "Sorting strategy: LATEST_VERSION, RELEVANCE, or COORDINATES. Defaults to LATEST_VERSION.", defaultValue = "LATEST_VERSION", example = "LATEST_VERSION")
    @Builder.Default
    private SortStrategy sortBy = SortStrategy.LATEST_VERSION;

    /** If true, collapses historical versions and returns only the newest version per artifact. */
    @Schema(description = "If true, collapses historical versions and returns only the newest version per unique artifact. Defaults to true.", defaultValue = "true", example = "true")
    @Builder.Default
    private Boolean collapseVersions = true;

    /** If true, filters out pre-release, milestone, release candidate, and snapshot versions. */
    @Schema(description = "Whether to filter out pre-release versions (alpha, beta, rc, milestone, preview, snapshot). Defaults to false.", defaultValue = "false", example = "false")
    @Builder.Default
    private Boolean stableOnly = false;

    /** Starting index for pagination (0-based). Defaults to 0. */
    @Schema(description = "Starting index for pagination (0-based). Defaults to 0.", defaultValue = "0", example = "0")
    @Builder.Default
    private Integer startIndex = 0;

    /** Maximum number of results to return per page. Defaults to 25. */
    @Schema(description = "Maximum number of results to return per page. Defaults to 25.", defaultValue = "25", example = "25")
    @Builder.Default
    private Integer pageSize = 25;
}
