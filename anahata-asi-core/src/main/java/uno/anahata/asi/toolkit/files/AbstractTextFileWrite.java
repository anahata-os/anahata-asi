/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * Base DTO for text file operations, providing common fields for path, 
 * historical content preservation, and optimistic locking.
 * 
 * @author anahata
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
@EqualsAndHashCode
public abstract class AbstractTextFileWrite {
    
    /**
     * The absolute path to the file to be updated.
     */
    @Schema(description = "The absolute path to the file.", required = true)
    protected String path;
    
    /**
     * The original content of the file before this operation was applied.
     * This is captured during the first render to support historical diff views.
     */
    @JsonIgnore
    @Schema(hidden = true)
    @Setter
    protected String originalContent;

    /**
     * Optimistic locking: the expected last modified timestamp of the file on disk.
     */
    @Schema(description = "Optimistic locking: the expected last modified timestamp of the file on disk.", required = true)
    protected long lastModified;

    /**
     * Minimal constructor for standard tool invocation and builder support.
     * 
     * @param path The file path.
     * @param lastModified The locking timestamp.
     */
    public AbstractTextFileWrite(String path, long lastModified) {
        this.path = path;
        this.lastModified = lastModified;
    }
}
