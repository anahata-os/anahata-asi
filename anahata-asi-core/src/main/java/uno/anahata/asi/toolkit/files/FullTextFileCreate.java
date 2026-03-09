/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import io.swagger.v3.oas.annotations.media.Schema;
import java.nio.file.Files;
import java.nio.file.Paths;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.tool.AiToolException;

/**
 * A rich DTO for creating a new text file. It encapsulates the path 
 * and the initial content.
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "A rich DTO for creating a new text file.")
public class FullTextFileCreate {
    
    /**
     * The absolute path to the file to be created.
     */
    @Schema(description = "The absolute path to the file.", required = true)
    private String path;

    /**
     * The full initial content for the new file.
     */
    @Schema(description = "The initial content for the file.", required = true)
    private String content;

    /**
     * Performs pre-flight validation of the creation operation.
     * 
     * @param agi The parent agi session.
     * @throws AiToolException if validation fails.
     */
    public void validate(Agi agi) throws AiToolException {
        if (Files.exists(Paths.get(path))) {
            throw new AiToolException("File already exists and cannot be created: " + path);
        }
    }

}
