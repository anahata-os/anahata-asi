/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.sun.source.tree.Tree;
import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serializable;
import java.util.List;
import java.util.Map;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.netbeans.api.java.source.WorkingCopy;

/**
 * Abstract base class for all structural Java refinement intents.
 * <p>
 * This class uses polymorphic JSON mapping to allow the ASI to send a list
 * of different structural operations in a single batch.
 * </p>
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
@JsonSubTypes({
    @JsonSubTypes.Type(value = InsertMemberIntent.class, name = "insert"),
    @JsonSubTypes.Type(value = UpdateMemberIntent.class, name = "update"),
    @JsonSubTypes.Type(value = DeleteMemberIntent.class, name = "delete"),
    @JsonSubTypes.Type(value = MoveMemberIntent.class, name = "move")
})
@Schema(description = "Represents a single structural AST modification instruction.")
public abstract class CodeRefinementIntent implements Serializable {

    /**
     * The fully qualified name (FQN) of the target class where this intent will be applied.
     * If left null or empty, the intent targets the file-level.
     */
    @Schema(description = "The FQN of the target class (e.g. 'com.foo.Bar'). Use '$' for nested types. Leave empty for file-level.")
    private String classFqn;

    /**
     * Authoritatively applies this intent to the provided working copy.
     * <p>
     * Instead of rewriting the WorkingCopy directly, intents update the 
     * {@code modifiedMembers} map. This map acts as a "Surgery Table" where 
     * structural containers (ClassTree, CompilationUnitTree) are mapped to 
     * their modified list of child Trees.
     * </p>
     * 
     * @param wc The active working copy.
     * @param modifiedMembers The accumulator for all changes in the batch.
     * @throws Exception if resolution or AST creation fails.
     */
    public abstract void apply(WorkingCopy wc, Map<Tree, List<Tree>> modifiedMembers, boolean optimize) throws Exception;
}
