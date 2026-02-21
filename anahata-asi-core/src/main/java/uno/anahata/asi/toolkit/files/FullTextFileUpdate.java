/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * A rich DTO for updating a text file with its full new content. It encapsulates the new content, 
 * optimistic locking metadata from the base class, and a set of line-level comments to provide 
 * explanations for specific changes.
 * 
 * @author anahata
 */
@Getter
@NoArgsConstructor
@Schema(description = "A rich DTO for updating a text file with full new content and line-level comments.")
public class FullTextFileUpdate extends AbstractTextFileWrite {
    /**
     * The full new content for the file.
     */
    @Schema(description = "The new content for the file.", required = true)
    private String newContent;


    /**
     * A list of comments associated with specific lines in the new content.
     */
    @Schema(description = "A list of comments for specific lines, intended for UI rendering.")
    private List<LineComment> lineComments;

    public FullTextFileUpdate(String path, long lastModified, String newContent, List<LineComment> lineComments) {
        super(path, lastModified);
        this.newContent = newContent;
        this.lineComments = lineComments;
    }
    
    
}
