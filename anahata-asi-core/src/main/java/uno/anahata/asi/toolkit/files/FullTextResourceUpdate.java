/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.tool.AiToolException;

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
    public FullTextResourceUpdate(String path, long lastModified, String newContent, List<LineComment> lineComments) {
        super(path, lastModified);
        this.newContent = newContent;
        this.lineComments = lineComments;
    }

    /** {@inheritDoc} */
    @Override
    public String calculateResultingContent(String currentContent) throws Exception {
        return newContent;
    }

    /** {@inheritDoc} */
    @Override
    public void validate(Agi agi) throws Exception {
        super.validate(agi);
        
        Resource r = agi.getResourceManager().get(resourceUuid);
        if (r != null) {
            try {
                // Ensure we are comparing against the latest physical state
                r.reloadIfNeeded();
                if (java.util.Objects.equals(r.asText(), newContent)) {
                    throw new AiToolException("Update rejected: The provided content is identical to the current file content on disk.");
                }
            } catch (AiToolException e) {
                throw e;
            } catch (Exception e) {
                // EXCEPTION POLICY: Strictly forbidding quiet catching. 
                // All read/reload failures during validation must be reported.
                throw new AiToolException("Failed to validate file identity for update: " + e.getMessage(), e);
            }
        }
    }
}
