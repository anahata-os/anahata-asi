/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.resources.text;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.Objects;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.internal.AnahataDiffUtils;

/**
 * Base DTO for text file operations, providing common fields for path, 
 * historical content preservation, and optimistic locking.
 * 
 * @author anahata
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
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
     * The name of the resource at the time of capture.
     */
    @JsonIgnore
    @Schema(hidden = true)
    @Setter
    protected String originalResourceName;

    /**
     * Optimistic locking: the expected last modified timestamp of the file on disk.
     */
    @Schema(description = "Optimistic locking: the expected last modified timestamp of the file on disk.", required = true)
    protected long lastModified;

    /**
     * Minimal constructor for standard tool invocation and builder support.
     * 
     * @param uuid The resource uuid.
     * @param lastModified The locking timestamp.
     */
    public AbstractTextResourceWrite(String uuid, long lastModified) {
        this.resourceUuid = uuid;
        this.lastModified = lastModified;
    }

    /**
     * Authoritatively captures the current state of the resource from the resource manager.
     * This is the mandatory first step before validation, calculation, or diffing.
     * 
     * @param agi The parent agi session.
     * @throws Exception if the resource cannot be resolved or is not textual.
     */
    public void captureOriginalContent(Agi agi) throws Exception {
        Resource res = agi.getResourceManager().getResources().get(resourceUuid);
        if (res == null) {
            throw new AgiToolException("Resource not found in context: " + resourceUuid);
        }
        if (!res.getHandle().isTextual()) {
             throw new AgiToolException("Resource is not a text resource: " + res.getName());
        }
        this.originalContent = res.asText();
        this.originalResourceName = res.getName();
    }

    /**
     * Calculates the resulting content of the resource based on the captured {@code originalContent}.
     * 
     * @param agi The parent agi session.
     * @return The resulting content.
     * @throws Exception if the calculation fails or original content is missing.
     */
    public abstract String calculateResultingContent(Agi agi) throws Exception;

    /**
     * Generates a unified diff of the proposed changes against the captured original content.
     * 
     * @param agi The parent agi session.
     * @return The unified diff string.
     * @throws Exception if original content is missing or diff generation fails.
     */
    public String getUnifiedDiff(Agi agi) throws Exception {
        if (originalContent == null) {
            throw new AgiToolException("Logic Error: getUnifiedDiff called before captureOriginalContent");
        }
        String proposed = calculateResultingContent(agi);
        return AnahataDiffUtils.generateUnifiedDiff(originalResourceName, originalContent, proposed);
    }

    /**
     * Performs pre-flight validation of the update operation. 
     * <b>Note:</b> This method ensures state is captured.
     * 
     * @param agi The parent agi session.
     * @throws Exception if validation fails.
     */
    public void validate(Agi agi) throws Exception {
        
        if (resourceUuid == null) {
            throw new AgiToolException("Resource uuid not provided");
        }
        
        Resource res = agi.getResourceManager().getResources().get(resourceUuid);
        if (res == null) {
            throw new AgiToolException("Resource not in context: " + resourceUuid);
        }
        
        // 1. Authoritative state capture
        captureOriginalContent(agi);

        // 2. Optimistic Locking Check
        long actualLm = res.getHandle().getLastModified();
        if (lastModified > 0 && lastModified != actualLm) {
            throw new AgiToolException("Optimistic locking failure for " + res.getName() + ". The time stamp provided doesn't match the last modified timestamp on disk: " + actualLm + " (provided: " + lastModified + ").");
        }

        // 3. Identical Content Check (Performed last to avoid masking more specific errors in subclasses)
        if (Objects.equals(originalContent, calculateResultingContent(agi))) {
             throw new AgiToolException("Update rejected: The resulting content is identical to the current file content on disk.");
        }
    }
}
