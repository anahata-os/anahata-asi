/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.resources.text.lines;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import uno.anahata.asi.agi.tool.AgiToolException;
import java.util.List;

/**
 * A safety-first deletion operation that removes a contiguous range of lines.
 * <p>
 * This implementation requires an explicit {@code expectedCount} to act as a
 * checksum, preventing accidental mass deletions due to coordinate drift or
 * misinterpretation of the file structure.
 * </p>
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(description = "Deletes a range of lines. Requires an explicit count to prevent off-by-one errors.")
public class LineDeletion extends AbstractLineEdit {

    /**
     * The 1-based line number where the deletion sequence begins (Inclusive).
     */
    @Schema(description = "The 1-based line number where deletion starts (Inclusive).", requiredMode = Schema.RequiredMode.REQUIRED)
    private int startLine;

    /**
     * The 1-based line number where the deletion sequence ends (Inclusive).
     */
    @Schema(description = "The 1-based line number where deletion ends (Inclusive).", requiredMode = Schema.RequiredMode.REQUIRED)
    private int endLine;

    /**
     * The number of lines expected to be removed.
     * <p>
     * This serves as a critical validation checkpoint; if the actual range
     * size does not match this value, the operation will fail to protect
     * file integrity.
     * </p>
     */
    @Schema(description = "The number of lines you expect to delete. Used as a checksum.", requiredMode = Schema.RequiredMode.REQUIRED)
    private int expectedCount;

    /**
     * {@inheritDoc}
     * <p>
     * Performs range validation and checksum verification before removing
     * lines from the provided list.
     * </p>
     */
    @Override
    public void apply(List<String> lines) throws AgiToolException {
        int actualCount = (endLine - startLine) + 1;
        if (actualCount != expectedCount) {
            throw new AgiToolException("Checksum failed: range [" + startLine + ", " + endLine + "] is " + actualCount + " lines, but you expected to delete " + expectedCount + ".");
        }

        if (startLine < 1 || endLine > lines.size()) {
            throw new AgiToolException("Deletion range out of bounds: [" + startLine + ", " + endLine + "] for file with " + lines.size() + " lines.");
        }

        int removeIndex = startLine - 1;
        for (int i = 0; i < actualCount; i++) {
            lines.remove(removeIndex);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the start line of the range to facilitate descending sort
     * during the application phase.
     * </p>
     */
    @Override
    public int getSortLine() {
        return startLine;
    }
}
