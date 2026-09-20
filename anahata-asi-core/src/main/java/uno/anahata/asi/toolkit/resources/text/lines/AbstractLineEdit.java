/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.resources.text.lines;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import uno.anahata.asi.agi.tool.AgiToolException;
import java.util.List;

/**
 * Base class for all surgical line-based edits.
 * 
 * @author anahata
 */
@Data
public abstract class AbstractLineEdit {
    
    /**
     * The semantic justification for this specific line mutation.
     * <p>
     * This field is utilized for logging, auditing, and providing human-readable
     * feedback in the UI during the resource application lifecycle.
     * </p>
     */
    @Schema(description = "The reason for this specific change.", requiredMode = Schema.RequiredMode.REQUIRED)
    protected String reason;

    /**
     * Applies this edit to a mutable list of lines.
     * 
     * @param lines The current lines of the file.
     * @throws AgiToolException if indices are out of bounds or validation fails.
     */
    public abstract void apply(List<String> lines) throws AgiToolException;

    /**
     * Returns the 1-based line number where this operation starts.
     * Used for sorting updates before application.
     * 
     * @return the start line.
     */
    public abstract int getSortLine();
}
