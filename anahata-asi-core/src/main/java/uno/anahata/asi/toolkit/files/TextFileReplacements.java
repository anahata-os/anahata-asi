/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import uno.anahata.asi.tool.AiToolException;

/**
 * Represents a set of text replacement operations for a specific file. Extends
 * AbstractTextFileWrite to inherit path and optimistic locking.
 *
 * @author anahata
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Getter
@EqualsAndHashCode(callSuper = true)
@Schema(description = "Represents a set of text replacement operations for a specific file.")
public class TextFileReplacements extends AbstractTextFileWrite {

    /**
     * The list of replacements to perform in this file.
     */
    @Schema(description = "The list of replacements to perform in this file.", required = true)
    private List<TextReplacement> replacements;

    @Builder
    public TextFileReplacements(String path, long lastModified, List<TextReplacement> replacements) {
        super(path, lastModified);
        this.replacements = replacements;
    }
    
    /**
     * Helper method to perform string replacements with validation.
     * 
     * @param currentContent The original content.
     * @return The updated content.
     * @throws AiToolException if a replacement fails.
     */
    public String performReplacements(String currentContent) throws AiToolException {
        String newContent = currentContent;
        for (TextReplacement replacement : replacements) {
            String target = replacement.getTarget();
            int count = StringUtils.countMatches(newContent, target);
            
            if (replacement.getExpectedCount() > 0 && count != replacement.getExpectedCount()) {
                throw new AiToolException("Replacement failed for '" + target + "'. Expected " + replacement.getExpectedCount() + " occurrences, but found " + count);
            }
            
            if (count == 0 && replacement.getExpectedCount() != 0) {
                 throw new AiToolException("Target string not found in file: " + target);
            }

            newContent = newContent.replace(target, replacement.getReplacement());
        }
        return newContent;
    }

}
