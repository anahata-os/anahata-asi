/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
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
public abstract class AbstractTextResourceWrite {
    
    /**
     * The absolute path to the file to be updated.
     */
    @Schema(description = "The resource uuid .", required = true)
    protected String resourceUuid;
    
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
    public AbstractTextResourceWrite(String uuid, long lastModified) {
        this.resourceUuid = uuid;
        this.lastModified = lastModified;
    }

    /**
     * Performs pre-flight validation of the update operation against the V2 resource context.
     * 
     * @param agi The parent agi session.
     * @throws Exception if validation fails.
     */
    public void validate(Agi agi) throws Exception {


        // 2. Resource Context Check - Operation MUST be performed on a managed resource
        Resource res = agi.getResourceManager().getResources().get(resourceUuid);
        if (res == null) {
             throw new AiToolException("No Resource in for uuid" + resourceUuid);
        }

        // 3. Capability Check
        if (!res.getHandle().isTextual()) {
             throw new AiToolException("Resource is not a text resource and cannot be updated via text tools: " + res.getName());
        }

        // 4. Optimistic Locking Check
        long actualLm = res.getHandle().getLastModified();
        if (lastModified > 0 && lastModified != actualLm) {
            throw new AiToolException("Optimistic locking failure for " + res.getName() + ". The time stamp provided doesnt match the last modified timestamp on disk:" + actualLm + ", you provided=" + lastModified + ").");
        }
    }
}
