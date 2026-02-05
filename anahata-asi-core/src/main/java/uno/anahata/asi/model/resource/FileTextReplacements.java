/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.resource;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Represents a set of text replacement operations for a specific file.
 * 
 * @author anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Represents a set of text replacement operations for a specific file.")
public class FileTextReplacements {

    /**
     * The absolute path to the file.
     */
    @Schema(description = "The absolute path to the file.", required = true)
    private String path;

    /**
     * The list of replacements to perform in this file.
     */
    @Schema(description = "The list of replacements to perform in this file.", required = true)
    private List<TextReplacement> replacements;

    /**
     * Optimistic locking: the expected last modified timestamp of the file on disk.
     */
    @Schema(description = "Optimistic locking: the expected last modified timestamp of the file on disk.", required = true)
    private long lastModified;
}
