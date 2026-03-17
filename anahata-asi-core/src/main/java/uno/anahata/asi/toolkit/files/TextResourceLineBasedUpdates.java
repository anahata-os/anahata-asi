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
     * <p>
     * <b>Model Habit Absorption:</b> This method intelligently absorbs a single trailing 
     * newline in {@code newContent}, treating it as structural rather than an 
     * instruction to create a blank line. This prevents the common "extra blank line" 
     * bug while still allowing intentional blank lines via double-newlines.
     * </p>
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
            
            // 1. Remove requested lines (Handle "remove remaining" for UI overrides)
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
            
            // 2. Insert replacement text with "Structural Newline Absorption"
            String content = lr.getNewContent();
            if (content != null && !content.isEmpty()) {
                List<String> newLines = new ArrayList<>(Arrays.asList(content.split("\\R", -1)));
                
                // ABSORPTION: If the content ends with a newline, split("-1") leaves a trailing empty string.
                // We remove it to sync with the model's "one newline per line" intuition.
                if (newLines.size() > 1 && newLines.get(newLines.size() - 1).isEmpty()) {
                    newLines.remove(newLines.size() - 1);
                }

                for (int i = newLines.size() - 1; i >= 0; i--) {
                    lines.add(listIndex, newLines.get(i));
                }
            }
        }
        
        return String.join(separator, lines);
    }

    /** {@inheritDoc} */
    @Override
    public String calculateResultingContent(String currentContent) throws Exception {
        return performUpdates(currentContent);
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
            if (lr.getStartLine() <= lastEndLine) {
                throw new AiToolException("Overlapping line updates detected at line " + lr.getStartLine());
            }
            // Technical precision: insertions (count=0) occupy the startLine point, 
            // removals/replacements occupy [startLine, startLine + count - 1]
            //lastEndLine = lr.getStartLine() + Math.max(0, lr.getLineCount() - 1);
            // OLD: lastEndLine = lr.getStartLine() + Math.max(0, lr.getLineCount() - 1);
            // NEW: Correctly handle insertions (count 0) so they don't "occupy" a line.
            lastEndLine = lr.getStartLine() + lr.getLineCount() - (lr.getLineCount() > 0 ? 1 : 0);
        }
    }
}
