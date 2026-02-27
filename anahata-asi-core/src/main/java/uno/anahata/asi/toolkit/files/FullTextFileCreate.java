/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

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

}
