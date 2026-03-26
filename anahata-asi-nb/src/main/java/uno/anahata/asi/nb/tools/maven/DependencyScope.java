/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.maven;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * The top-level container that groups {@link DependencyGroup}s by their Maven scope.
 * <p>
 * This represents the primary grouping level for declared dependencies, separating 
 * artifacts into logical buckets like {@code compile}, {@code test}, or {@code provided}.
 * </p>
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "A container that groups dependency information by scope.")
public class DependencyScope {
    
    @Schema(description = "The dependency scope (e.g., compile, test, provided).", example = "compile")
    private String scope;
    
    @Schema(description = "The list of dependency groups belonging to this scope.")
    private List<DependencyGroup> groups;
}