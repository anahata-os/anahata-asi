/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.resources.text.lines;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import uno.anahata.asi.agi.tool.AgiToolException;
import java.util.Arrays;
import java.util.List;

/**
 * VISUAL TARGET REPLACEMENT: Replaces a range of lines with new content.
 * <p>
 * Use this when you can clearly see the start and end of a block (like a method or Javadoc)
 * in the RAG message.
 * </p>
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(description = "Replaces a specific range of lines [startLine, endLine] inclusive."
        + " Use `startLine`=`endLine` to replace a single line. All lines from `startLine` to `endLine` will be replaced with `content`."
        + " The `content` can contain any number of lines. Never include in content lines that do not need to change and make sure that startLine is exactly the first line that needs to be replaced. \n"
        + "STRICT RULE: If the first line of your content is identical to the original line at startLine, or the last line is identical to endLine, your range is too wide. Shrink the range to only include lines with actual character changes.")
public class LineReplacement extends AbstractLineEdit {

    /**
     * The 1-based line number of the resource in the RAG message where the
     * replacement starts (Inclusive).
     */
    @Schema(description = "The 1-based line number of the resource in the RAG message where the replacement starts (Inclusive).", required = true)
    private int startLine;

    /**
     * The 1-based line number of the resource in the RAG message where the
     * replacement ends (Inclusive).
     */
    @Schema(description = "The 1-based line number of the resource in the RAG message where the replacement ends (Inclusive).", required = true)
    private int endLine;

    /**
     * The new content for the target range.
     * <p>
     * This can contain any number of lines. Implementation details: Trailing
     * newlines are processed structurally to ensure proper absorption.
     * </p>
     */
    @Schema(description = "The new content for the [startLine, endLine] range of the resource in the RAG message. It can have as many lines as you want. Do not include surrounding anchors because that is not the way this tool works.", required = true)
    private String content;

    /**
     * {@inheritDoc}
     * <p>
     * Performs range validation before removing the target lines and splicing
     * in the new content.
     * </p>
     */
    @Override
    public void apply(List<String> lines) throws AgiToolException {
        if (startLine < 1 || endLine < startLine || endLine > lines.size()) {
            throw new AgiToolException("Replacement range [" + startLine + ", " + endLine + "] is invalid or out of bounds (1-" + lines.size() + ")");
        }

        // 1. Remove the old lines
        int countToRemove = (endLine - startLine) + 1;
        int removeIndex = startLine - 1;
        for (int i = 0; i < countToRemove; i++) {
            lines.remove(removeIndex);
        }

        // 2. Insert new content (with absorption)
        if (content != null && !content.isEmpty()) {
            List<String> newLines = new java.util.ArrayList<>(Arrays.asList(content.split("\\R", -1)));
            if (newLines.size() > 1 && newLines.get(newLines.size() - 1).isEmpty()) {
                newLines.remove(newLines.size() - 1);
            }
            for (int i = newLines.size() - 1; i >= 0; i--) {
                lines.add(removeIndex, newLines.get(i));
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the start line of the replacement range for sorting.
     * </p>
     */
    @Override
    public int getSortLine() {
        return startLine;
    }
}
