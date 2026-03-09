/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Optional;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.tool.AiToolException;

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

    /**
     * Performs pre-flight validation of the update operation against the V2 resource context.
     * 
     * @param agi The parent agi session.
     * @throws AiToolException if validation fails.
     */
    public void validate(Agi agi) throws AiToolException {
        Path p = Paths.get(path);
        
        // 1. Basic Path Validation
        if (!Files.exists(p)) {
             throw new AiToolException("File not found on host filesystem: " + path);
        }

        // 2. Resource Context Check - Operation MUST be performed on a managed resource
        Optional<Resource> res = agi.getResourceManager2().findByUri(p.toUri().toString());
        if (res.isEmpty()) {
             throw new AiToolException("Resource is not in context. You must load the file before attempting to update it: " + path);
        }

        // 3. Capability Check
        if (!res.get().getHandle().isTextual()) {
             throw new AiToolException("Resource is not a text resource and cannot be updated via text tools: " + path);
        }

        // 4. Optimistic Locking Check
        long actualLm = res.get().getHandle().getLastModified();
        if (lastModified > 0 && lastModified != actualLm) {
            throw new AiToolException("Optimistic locking failure for " + path + ". The file has been modified by another process since it was last loaded (disk=" + actualLm + ", expected=" + lastModified + "). Please re-read the file.");
        }
    }
}
