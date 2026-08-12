/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * A recursive DTO representing a node in the Java type hierarchy inside IntelliJ IDEA.
 * It uses {@link JavaType} as the unique identity for each node, ensuring 
 * that every level of the tree is actionable (members can be queried, sources read, etc.).
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "Represents a node in a Java type hierarchy tree.")
public class JavaHierarchyNode {

    /** The JavaType identity for this node. */
    @Schema(description = "The JavaType keychain for this node.")
    private JavaType type;

    /** Recursive list of types that this type extends or implements. */
    @Builder.Default
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    @Schema(description = "Recursive list of types that this type extends or implements.")
    private List<JavaHierarchyNode> supertypes = new ArrayList<>();

    /** Recursive list of types that extend or implement this type. */
    @Builder.Default
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    @Schema(description = "Recursive list of types that extend or implement this type.")
    private List<JavaHierarchyNode> subtypes = new ArrayList<>();
}
