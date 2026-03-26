/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.maven;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * A container that groups declared Maven artifacts by their common {@code groupId}.
 * <p>
 * This structure is used by the {@link Maven} toolkit to provide a hierarchical 
 * and token-efficient representation of project dependencies.
 * </p>
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "A container that groups declared artifacts by their common groupId.")
public class DependencyGroup {
    
    @Schema(description = "The common groupId for all artifacts in this group.", example = "org.apache.commons")
    private String id;
    
    @Schema(description = "The list of artifacts belonging to this group.")
    private List<DeclaredArtifact> artifacts;
}