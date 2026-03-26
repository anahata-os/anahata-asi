/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.maven;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * The top-level container for resolved dependencies, grouping them by Maven scope.
 * <p>
 * This is an ultra-compact version of {@link DependencyScope}, optimized for 
 * large project classpaths where token count is critical. It delegates to 
 * {@link ResolvedDependencyGroup} for a minimalist representation of artifacts.
 * </p>
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "A container that groups fully resolved dependencies by scope in an ultra-compact format.")
public class ResolvedDependencyScope {
    
    @Schema(description = "The dependency scope (e.g., compile, test, provided).", example = "compile")
    private String scope;
    
    @Schema(description = "The list of dependency groups belonging to this scope.")
    private List<ResolvedDependencyGroup> groups;
}