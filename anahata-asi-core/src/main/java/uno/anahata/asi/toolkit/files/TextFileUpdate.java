/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * A rich DTO for updating a text file. It encapsulates the new content, 
 * optimistic locking metadata, and a set of line-level comments to provide 
 * explanations for specific changes.
 * 
 * @author anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "A rich DTO for updating a text file with metadata and line-level comments.")
public class TextFileUpdate {

    /**
     * The absolute path to the file to be updated.
     */
    @Schema(description = "The absolute path to the file.", required = true)
    private String path;

    /**
     * The full new content for the file.
     */
    @Schema(description = "The new content for the file.", required = true)
    private String newContent;

    /**
     * Optimistic locking: the expected last modified timestamp of the file on disk.
     */
    @Schema(description = "Optimistic locking: the expected last modified timestamp of the file on disk.", required = true)
    private long lastModified;

    /**
     * A list of comments associated with specific lines in the new content.
     */
    @Schema(description = "A list of comments for specific lines, intended for UI rendering.")
    private List<LineComment> lineComments;
}
