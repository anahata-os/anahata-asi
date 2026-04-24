/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * Intent to update an existing structural member.
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Schema(description = "Instruction to update an existing structural member.")
public class UpdateMemberIntent extends CodeRefinementIntent {

    @Schema(description = "The ABSOLUTE FQN of the member to update (e.g. 'com.foo.Bar.myMethod()').", required = true)
    private String memberFqn;

    @Schema(description = "The new member declaration (signature). If omitted, the existing signature is preserved.")
    private String declaration;

    @Schema(description = "The new WHOLE body code. For methods, logic inside braces. For fields, the initializer expression (part after '='). If omitted, existing body is preserved.")
    private String body;

}
