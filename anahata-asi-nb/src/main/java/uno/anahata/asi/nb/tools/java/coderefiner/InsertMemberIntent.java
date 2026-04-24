/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import uno.anahata.asi.nb.tools.java.JavaSourceUtils.RelativePosition;

/**
 * Intent to insert a new member (method, field, inner type) into a class or file.
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Schema(description = "Instruction to insert a new structural member.")
public class InsertMemberIntent extends CodeRefinementIntent {

    @Schema(description = "The full member declaration (e.g. '@Override public void foo()' or 'private String name').", required = true)
    private String declaration;

    @Schema(description = "The WHOLE body code. For methods, logic inside braces. For fields, the initializer expression (the part after the '=').")
    private String body;

    @Schema(description = "Position relative to the anchor member.", required = true)
    private RelativePosition position;

    @Schema(description = "Anchor member name relative to class (e.g. 'myMethod()'). Mandatory for BEFORE/AFTER.")
    private String anchorMemberName;
}
