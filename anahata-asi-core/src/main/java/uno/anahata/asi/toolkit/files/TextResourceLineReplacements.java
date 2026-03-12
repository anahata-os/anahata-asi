/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.tool.AiToolException;

/**
 * Represents a set of line-based replacements in a text file.
 * This tool is ideal for precisely targeting known line ranges.
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Schema(description = "Represents a set of line-based replacements in a text file.")
@Slf4j
public class TextResourceLineReplacements extends AbstractTextResourceWrite {

    /**
     * The list of line-based replacements.
     */
    @Schema(description = "The list of line-based replacements.", required = true)
    private List<LineReplacement> replacements;

    @Builder
    public TextResourceLineReplacements(String path, long lastModified, List<LineReplacement> replacements) {
        super(path, lastModified);
        this.replacements = replacements;
    }

    /**
     * Applies the line-based replacements to the given content.
     * 
     * @param currentContent The original file content.
     * @return The modified content.
     * @throws AiToolException if replacements overlap or line numbers are out of bounds.
     */
    public String performReplacements(String currentContent) throws AiToolException {
        // Use a list of lines for easier manipulation. 
        // We split with -1 to preserve trailing empty lines/newcasting.
        List<String> lines = new ArrayList<>(Arrays.asList(currentContent.split("\\R", -1)));
        
        // 1. Sort replacements descending by startLine to maintain index integrity
        List<LineReplacement> sorted = new ArrayList<>(replacements);
        sorted.sort(Comparator.comparingInt(LineReplacement::getStartLine).reversed());
        
        // 2. Validate and apply
        for (LineReplacement lr : sorted) {
            int start = lr.getStartLine();
            int count = lr.getLineCount();
            
            if (start < 1 || start > lines.size() + 1) {
                throw new AiToolException("Start line " + start + " is out of bounds (1-" + (lines.size() + 1) + ")");
            }
            
            int listIndex = start - 1;
            
            // Remove requested lines
            for (int i = 0; i < count; i++) {
                if (listIndex < lines.size()) {
                    lines.remove(listIndex);
                } else {
                    throw new AiToolException("Cannot remove " + count + " lines starting at " + start + ". End of file reached prematurely.");
                }
            }
            
            // Insert replacement text
            if (lr.getReplacement() != null && !lr.getReplacement().isEmpty()) {
                String[] newLines = lr.getReplacement().split("\\R", -1);
                for (int i = newLines.length - 1; i >= 0; i--) {
                    lines.add(listIndex, newLines[i]);
                }
            }
        }
        
        return String.join("\n", lines);
    }

    /** {@inheritDoc} */
    @Override
    public void validate(Agi agi) throws Exception {
        super.validate(agi);
        
        // Check for overlapping line ranges
        List<LineReplacement> sorted = new ArrayList<>(replacements);
        sorted.sort(Comparator.comparingInt(LineReplacement::getStartLine));
        
        int lastEndLine = -1;
        for (LineReplacement lr : sorted) {
            log.info("Validating replacement: {}", lr);
            if (lr.getStartLine() <= lastEndLine) {
                throw new AiToolException("Overlapping line replacements detected at line " + lr.getStartLine());
            }
            lastEndLine = lr.getStartLine() + Math.max(0, lr.getLineCount() - 1);
        }
    }
}
