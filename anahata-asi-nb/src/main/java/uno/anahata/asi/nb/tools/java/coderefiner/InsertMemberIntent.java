/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import com.sun.source.tree.ClassTree;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.Tree;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.netbeans.api.java.source.GeneratorUtilities;
import org.netbeans.api.java.source.WorkingCopy;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.nb.tools.java.JavaSourceUtils;
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

    /**
     * {@inheritDoc}
     * <p>Implementation logic: Resolves the target container (Class or CU) using 
     * identity-safe resolution, parses the new member using a memory-backed AST, 
     * and inserts it into the Surgery Table.</p>
     */
    @Override
    public void apply(WorkingCopy wc, Map<Tree, List<Tree>> modifiedMembers, boolean optimize) throws Exception {
        GeneratorUtilities gu = GeneratorUtilities.get(wc);
        
        // 1. Parse and optionally optimize the new member FIRST.
        // This may add imports to the CompilationUnit.
        Tree newMember = CodeRefinementBatch.parseMember(wc, declaration, body);
        if (optimize) {
            newMember = gu.importFQNs(newMember);
        }

        // 2. Resolve the parent container AFTER optimization.
        // This ensures we have the latest tree instance from the (possibly updated) CU.
        Tree parentTree;
        String classFqn = getClassFqn();
        if (classFqn == null || classFqn.isBlank()) {
            parentTree = wc.getCompilationUnit();
        } else {
            parentTree = CodeRefinementBatch.findMemberInWorkingCopy(wc, classFqn);
            if (parentTree == null) {
                throw new AgiToolException("Target class not found in current source: " + classFqn);
            }
        }

        // 3. Update the Surgery Table
        List<Tree> members = modifiedMembers.computeIfAbsent(parentTree, p -> {
            if (p instanceof ClassTree ct) {
                return new ArrayList<>(ct.getMembers());
            } else {
                return new ArrayList<>(((CompilationUnitTree) p).getTypeDecls());
            }
        });

        int insertIdx = CodeRefinementBatch.getInsertIndex(wc, members, position, anchorMemberName);
        members.add(insertIdx, newMember);
    }
}
