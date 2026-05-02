/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.resources.text;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * Represents a single text replacement operation within a file.
 * 
 * @author anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Represents a single text replacement operation.")
public class TextReplacement {

    /**
     * The exact string to be replaced.
     */
    @Schema(description = "The exact string to be replaced.", required = true)
    private String target;

    /**
     * The replacement string.
     */
    @Schema(description = "The replacement string.", required= true, requiredMode = Schema.RequiredMode.REQUIRED)
    private String replacement;

    /**
     * A detailed explanation of why this replacement is being made.
     */
    @Schema(description = "The reason for this change.")
    private String reason;

    /**
     * The total number of times the target string must appear in the file.
     * This acts as a checksum to ensure context integrity.
     */
    @Schema(description = "The MANDATORY total count of matches for the target string in the file. Must be provided and it must match exactly the total occurrences of the target string in the file, even if it is only 1.", required = true, requiredMode = Schema.RequiredMode.REQUIRED)
    private int totalOccurrences;

    /**
     * The 1-based indices of the specific occurrences to replace.
     * If null or empty, ALL matching occurrences will be replaced.
     */
    @Schema(description = "The 1-based indices of the specific occurrences to replace (e.g. [1, 3]). If null or empty, all occurrences are replaced.")
    private List<Integer> occurrenceIndexes;
}
