/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.resources.text;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.tool.AgiToolException;

/**
 * A rich DTO for updating a text file with its full new content. It encapsulates the new content, 
 * optimistic locking metadata from the base class, and a set of line-level comments to provide 
 * explanations for specific changes.
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Schema(description = "A rich DTO for updating a text file with full new content and line-level comments.")
public class FullTextResourceUpdate extends AbstractTextResourceWrite {
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

    @Builder
    public FullTextResourceUpdate(String resourceUuid, long lastModified, String newContent, List<LineComment> lineComments) {
        super(resourceUuid, lastModified);
        this.newContent = newContent;
        this.lineComments = lineComments;
    }

    /** {@inheritDoc} */
    @Override
    public String calculateResultingContent(Agi agi) throws Exception {
        if (originalContent == null) {
            throw new AgiToolException("Logic Error: calculateResultingContent called before captureOriginalContent");
        }
        return newContent;
    }

    /** {@inheritDoc} */
    @Override
    public void validate(Agi agi) throws Exception {
        super.validate(agi);
        // Identical content check is now in parent validate()
    }
}
