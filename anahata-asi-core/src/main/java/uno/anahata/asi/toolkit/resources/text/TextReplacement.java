/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.resources.text;

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
     * <ul>
     *   <li><b>n > 0</b>: Must match exactly n occurrences.</li>
     *   <li><b>0</b>: Must match exactly 0 occurrences (ensures absence).</li>
     *   <li><b>-1</b>: Default. Match at least 1 occurrence (Strict).</li>
     *   <li><b>-2</b>: Match 0 or more occurrences (Lenient/Optional).</li>
     * </ul>
     */
    @Builder.Default
    @Schema(description = "The expected number of occurrences to replace. Use -1 for strictly 1+, -2 for 0+.", defaultValue = "-1")
    private int expectedCount = -1;
}
