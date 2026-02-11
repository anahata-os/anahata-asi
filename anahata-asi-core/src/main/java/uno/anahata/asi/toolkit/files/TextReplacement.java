/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

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
    @Schema(description = "The replacement string.", required = true)
    private String replacement;

    /**
     * A detailed explanation of why this replacement is being made.
     */
    @Schema(description = "The reason for this change.")
    private String reason;

    /**
     * The expected number of occurrences of the target string in the file.
     * If set to a value greater than 0, the operation will fail if the 
     * actual number of replacements does not match this value.
     * Use -1 to replace all occurrences without checking the count.
     */
    @Builder.Default
    @Schema(description = "The expected number of occurrences to replace. Use -1 for all.", defaultValue = "-1")
    private int expectedCount = -1;
}
