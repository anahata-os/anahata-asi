/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner.polymorphic;

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
@Schema(description = "Represents a single structural AST modification instruction."
        + " The CodeRefinementIntent abstract class has 4 concrete subtypes: InsertMemberIntent, UpdateMemberIntent, DeleteMemberIntent and MoveMemberIntent,  Check out the schema carefully and use the polymorphic discriminator attribute 'type' accordingly so the framework can map your json intents to the correct concrete java subtypes.")
public abstract class CodeRefinementIntentPolymorphic implements Serializable {

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
     * @param optimize Whether to optimize imports during this batch.
     * @throws Exception if resolution or AST creation fails.
     */
    public abstract void apply(WorkingCopy wc, Map<Tree, List<Tree>> modifiedMembers, boolean optimize) throws Exception;

    /**
     * Returns a human-readable HTML description of the intent for display in the UI.
     * 
     * @return A pretty HTML string (e.g., "[+] <b>Insert Field</b> END").
     */
    public abstract String getHtmlDisplay();

    /**
     * Helper to extract the simple name from a potentially complex FQN.
     *
     * @param fqn The FQN to extract the simple name from.
     * @return The simple name of the FQN.
     */
    protected String getSimpleName(String fqn) {
        if (fqn == null || fqn.isBlank()) {
            return "Unknown";
        }
        int paren = fqn.indexOf('(');
        String namePart = paren == -1 ? fqn : fqn.substring(0, paren);
        int lastDot = Math.max(namePart.lastIndexOf('.'), namePart.lastIndexOf('$'));
        return lastDot == -1 ? namePart : namePart.substring(lastDot + 1);
    }
}
