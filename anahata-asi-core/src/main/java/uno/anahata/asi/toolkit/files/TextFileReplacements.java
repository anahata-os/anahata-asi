/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import io.swagger.v3.oas.annotations.media.Schema;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.tool.AiToolException;

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

    /** {@inheritDoc} 
     * Validates the replacements against the current state of the resource handle.
     */
    @Override
    public void validate(Agi agi) throws AiToolException {
        super.validate(agi);
        
        Optional<Resource> res = agi.getResourceManager2().findByUri(Paths.get(path).toUri().toString());
        try (InputStream is = res.get().getHandle().openStream()) {
            String content = IOUtils.toString(is, res.get().getHandle().getCharset());
            performReplacements(content); // Test replacements to validate occurrences
        } catch (AiToolException ae) {
            throw ae;
        } catch (Exception e) {
            throw new AiToolException("Replacement validation failed: " + e.getMessage());
        }
    }

}
