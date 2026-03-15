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
@Schema(description = "Represents a set of line-based updates in a text file.")
@Slf4j
public class TextResourceLineBasedUpdates extends AbstractTextResourceWrite {

    /**
     * The list of line-based updates.
     */
    @Schema(description = "The list of line-based updates.", required = true)
    private List<LineBasedUpdate> updates;

    @Builder
    public TextResourceLineBasedUpdates(String uuid, long lastModified, List<LineBasedUpdate> replacements) {
        super(uuid, lastModified);
        this.updates = replacements;
    }

    /**
     * Applies the line-based updates to the given content.
     * 
     * @param currentContent The original file content.
     * @return The modified content.
     * @throws AiToolException if updates overlap or line numbers are out of bounds.
     */
    public String performUpdates(String currentContent) throws AiToolException {
        String separator = currentContent.contains("\r\n") ? "\r\n" : "\n";
        List<String> lines = new ArrayList<>(Arrays.asList(currentContent.split("\\R", -1)));
        
        // Sort descending by startLine to maintain index integrity during mutation
        List<LineBasedUpdate> sorted = new ArrayList<>(updates);
        sorted.sort(Comparator.comparingInt(LineBasedUpdate::getStartLine)
                .thenComparingInt(LineBasedUpdate::getLineCount).reversed());
        
        for (LineBasedUpdate lr : sorted) {
            int start = lr.getStartLine();
            int count = lr.getLineCount();
            
            if (start < 1 || start > lines.size() + 1) {
                throw new AiToolException("Start line " + start + " is out of bounds (1-" + (lines.size() + 1) + ")");
            }
            
            int listIndex = start - 1;
            
            // Remove requested lines (Handle "remove remaining" for UI overrides)
            for (int i = 0; i < count; i++) {
                if (listIndex < lines.size()) {
                    lines.remove(listIndex);
                } else {
                    if (count != Integer.MAX_VALUE) {
                        throw new AiToolException("Cannot remove " + count + " lines starting at " + start + ". End of file reached prematurely.");
                    }
                    break;
                }
            }
            
            // Insert replacement text
            if (lr.getNewContent() != null && !lr.getNewContent().isEmpty()) {
                String[] newLines = lr.getNewContent().split("\\R", -1);
                for (int i = newLines.length - 1; i >= 0; i--) {
                    lines.add(listIndex, newLines[i]);
                }
            }
        }
        
        return String.join(separator, lines);
    }

    /** {@inheritDoc} */
    @Override
    public void validate(Agi agi) throws Exception {
        super.validate(agi);
        
        if (updates == null || updates.isEmpty()) {
            throw new AiToolException("No replacements provided.");
        }

        List<LineBasedUpdate> sorted = new ArrayList<>(updates);
        sorted.sort(Comparator.comparingInt(LineBasedUpdate::getStartLine));
        
        int lastEndLine = -1;
        for (LineBasedUpdate lr : sorted) {
            log.info("Validating startLine={} lineCount={}, newContent='{}'", lr.getStartLine(), lr.getLineCount(), lr.getNewContent());
            if (lr.getStartLine() <= lastEndLine) {
                throw new AiToolException("Overlapping line updates detected at line " + lr.getStartLine());
            }
            // Technical precision: insertions (count=0) occupy the startLine point, 
            // removals/replacements occupy [startLine, startLine + count - 1]
            lastEndLine = lr.getStartLine() + Math.max(0, lr.getLineCount() - 1);
        }
    }
}
