/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.maven;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * A data transfer object representing a Lucene-compatible query clause for searching Maven indexes.
 * <p>
 * This DTO supports both leaf field clauses (such as matching a prefix on groupId or artifactId)
 * and composite boolean query trees (such as AND of ORs, or NOT exclusions) via nested {@code subClauses}.
 * </p>
 * 
 * @author anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "A structured query clause for searching Maven indexes, supporting field targeting, matching modes, and nested boolean clauses.")
public class MavenQueryClause {

    /**
     * Lucene boolean occurrence operator determining how a clause must match.
     */
    @Schema(description = "Lucene boolean occurrence operator: MUST (+), SHOULD ( ), MUST_NOT (-), or FILTER.")
    public enum Occur {
        /** Clause must match (Boolean AND / +). */
        @Schema(description = "Clause must match (Boolean AND / +).")
        MUST,

        /** Clause should match (Boolean OR / optional). */
        @Schema(description = "Clause should match (Boolean OR / optional).")
        SHOULD,

        /** Clause must not match (Boolean NOT / -). */
        @Schema(description = "Clause must not match (Boolean NOT / -).")
        MUST_NOT,

        /** Clause must match as a non-scoring filter. */
        @Schema(description = "Clause must match as a non-scoring filter.")
        FILTER
    }

    /**
     * Match mode specifying how the value string is matched against Lucene tokens.
     */
    @Schema(description = "Match mode specifying how the value string is matched against index terms: EXACT, PREFIX, or WILDCARD.")
    public enum Match {
        /** Exact term match against the indexed token. */
        @Schema(description = "Exact term match against the indexed token.")
        EXACT,

        /** Prefix match (e.g. 'org.apache.*'). */
        @Schema(description = "Prefix match (e.g. 'org.apache.*').")
        PREFIX,

        /** Wildcard pattern match with '*' or '?' (e.g. '*runner*'). */
        @Schema(description = "Wildcard pattern match with '*' or '?' (e.g. '*runner*').")
        WILDCARD
    }

    /**
     * Target index field to match against.
     */
    @Schema(description = "The target Maven index field to query.")
    public enum TargetField {
        /** Search across all primary metadata fields (groupId, artifactId, version, name, description, classes). */
        @Schema(description = "Search across all primary metadata fields (groupId, artifactId, version, name, description, classes).")
        ALL,

        /** Artifact groupId (e.g. 'org.apache.commons'). */
        @Schema(description = "Artifact groupId (e.g. 'org.apache.commons').")
        GROUP_ID,

        /** Artifact artifactId (e.g. 'commons-lang3'). */
        @Schema(description = "Artifact artifactId (e.g. 'commons-lang3').")
        ARTIFACT_ID,

        /** Artifact version (e.g. '3.12.0'). */
        @Schema(description = "Artifact version (e.g. '3.12.0').")
        VERSION,

        /** Artifact packaging type (e.g. 'jar', 'nbm', 'pom', 'maven-plugin'). */
        @Schema(description = "Artifact packaging type (e.g. 'jar', 'nbm', 'pom', 'maven-plugin').")
        PACKAGING,

        /** Artifact display or project name. */
        @Schema(description = "Artifact display or project name.")
        NAME,

        /** Artifact project description. */
        @Schema(description = "Artifact project description.")
        DESCRIPTION,

        /** Fully qualified class names or simple class names contained within the artifact archive. */
        @Schema(description = "Fully qualified class names or simple class names contained within the artifact archive.")
        CLASSES
    }

    /** The target field to query. Defaults to ALL. */
    @Schema(description = "The target Maven index field. Defaults to ALL.", defaultValue = "ALL", example = "ARTIFACT_ID")
    @Builder.Default
    private TargetField field = TargetField.ALL;

    /** The keyword, prefix, or pattern to match against the field. */
    @Schema(description = "The keyword, prefix, or wildcard value to search for.", example = "commons-lang3")
    private String value;

    /** The match mode for evaluating value against the index. Defaults to PREFIX. */
    @Schema(description = "The matching strategy (EXACT, PREFIX, WILDCARD). Defaults to PREFIX.", defaultValue = "PREFIX", example = "PREFIX")
    @Builder.Default
    private Match match = Match.PREFIX;

    /** The Lucene occurrence modifier for this clause. Defaults to MUST. */
    @Schema(description = "Lucene occurrence operator: MUST (+), SHOULD ( ), MUST_NOT (-), or FILTER. Defaults to MUST.", defaultValue = "MUST", example = "MUST")
    @Builder.Default
    private Occur occur = Occur.MUST;

    /** Nested sub-clauses for composite boolean queries (AND of ORs, NOT groups, etc.). */
    @Schema(description = "Optional list of nested sub-clauses for composite boolean expressions. If specified, this clause acts as a BooleanQuery group.")
    private List<MavenQueryClause> subClauses;
}
