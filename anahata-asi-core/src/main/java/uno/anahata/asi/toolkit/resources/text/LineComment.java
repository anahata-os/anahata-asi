/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.resources.text;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Represents a comment associated with a specific line in a text file.
 * This is used to provide context and explanation for specific changes 
 * within the ASI's rich diff viewer.
 * 
 * @author anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Represents a comment associated with a specific line in a text file.")
public class LineComment {

    /**
     * The 1-based line number that this comment refers to.
     */
    @Schema(description = "The 1-based line number.", requiredMode = Schema.RequiredMode.REQUIRED)
    private int lineNumber;

    /**
     * The text content of the comment.
     */
    @Schema(description = "The comment text.", requiredMode = Schema.RequiredMode.REQUIRED)
    private String comment;
}
