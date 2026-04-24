/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import uno.anahata.asi.nb.tools.java.JavaSourceUtils.RelativePosition;

/**
 * Intent to move an existing member to a new position.
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Schema(description = "Instruction to move a member within its enclosing type.")
public class MoveMemberIntent extends CodeRefinementIntent {

    @Schema(description = "The ABSOLUTE FQN of the member to move.", required = true)
    private String memberFqn;

    @Schema(description = "The new position relative to the anchor.", required = true)
    private RelativePosition position;

    @Schema(description = "The anchor member name. Mandatory for BEFORE/AFTER.")
    private String anchorMemberName;

}
