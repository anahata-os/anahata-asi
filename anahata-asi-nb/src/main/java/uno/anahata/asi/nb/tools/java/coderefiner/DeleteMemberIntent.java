/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * Intent to delete an existing structural member.
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Schema(description = "Instruction to delete a structural member.")
public class DeleteMemberIntent extends CodeRefinementIntent {

    @Schema(description = "The ABSOLUTE FQN of the member to delete.", required = true)
    private String memberFqn;

}
