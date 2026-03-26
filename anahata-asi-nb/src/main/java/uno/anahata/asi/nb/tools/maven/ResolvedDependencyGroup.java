/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.maven;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * A container that groups fully resolved artifacts by their common {@code groupId}.
 * <p>
 * This is an ultra-compact version of {@link DependencyGroup}, specifically optimized 
 * for token efficiency in the RAG context. Instead of a list of DTOs, it holds a 
 * simple list of compact strings representing the artifact coordinates 
 * (artifactId:version[:classifier][:type]).
 * </p>
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "A container that groups fully resolved artifacts by their common groupId in an ultra-compact format.")
public class ResolvedDependencyGroup {
    
    @Schema(description = "The common groupId for all artifacts in this group.", example = "org.apache.commons")
    private String id;
    
    @Schema(description = "The list of compact artifact IDs (artifactId:version[:classifier][:type]) belonging to this group.", 
            example = "[\"commons-lang3:3.12.0\", \"commons-io:2.11.0\"]")
    private List<String> artifacts;
}