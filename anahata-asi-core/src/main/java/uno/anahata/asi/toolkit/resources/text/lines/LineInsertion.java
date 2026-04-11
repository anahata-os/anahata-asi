/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.resources.text.lines;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import uno.anahata.asi.agi.tool.AgiToolException;
import java.util.Arrays;
import java.util.List;

/**
 * A pure insertion operation that places new content before a specified line.
 * <p>
 * This operation follows a 'push-down' model: the original line at the target
 * coordinate and all subsequent lines are shifted downwards. This ensures zero
 * risk of accidentally deleting existing code.
 * </p>
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(description = "Performs a 'push-down' insertion. The original line at 'atLine' and all subsequent lines are pushed down.")
public class LineInsertion extends AbstractLineEdit {

    /**
     * The 1-based line number before which the content will be inserted.
     */
    @Schema(description = "The 1-based line number before which the content will be inserted.", required = true)
    private int atLine;

    /**
     * The raw text content to be inserted.
     * <p>
     * Trailing newlines are treated structurally to ensure proper line-break
     * absorption during the application phase.
     * </p>
     */
    @Schema(description = "The new lines to insert.", required = true)
    private String content;

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Splices the new lines into the list at the
     * calculated target index, preserving surrounding content.
     * </p>
     */
    @Override
    public void apply(List<String> lines) throws AgiToolException {
        if (atLine < 1 || atLine > lines.size() + 1) {
            throw new AgiToolException("Insertion line " + atLine + " is out of bounds (1-" + (lines.size() + 1) + ")");
        }
        
        if (content == null || content.isEmpty()) {
            return;
        }

        // Split into lines, treating trailing newline as structural (absorption)
        List<String> newLines = new java.util.ArrayList<>(Arrays.asList(content.split("\\R", -1)));
        if (newLines.size() > 1 && newLines.get(newLines.size() - 1).isEmpty()) {
            newLines.remove(newLines.size() - 1);
        }

        int targetIndex = atLine - 1;
        for (int i = newLines.size() - 1; i >= 0; i--) {
            lines.add(targetIndex, newLines.get(i));
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the insertion point coordinate for sorting.
     * </p>
     */
    @Override
    public int getSortLine() {
        return atLine;
    }
}
