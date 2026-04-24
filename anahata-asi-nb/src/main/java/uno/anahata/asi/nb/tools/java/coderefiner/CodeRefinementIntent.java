/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serializable;
import lombok.Data;
import lombok.NoArgsConstructor;

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

}
