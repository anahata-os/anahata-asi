/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner.polymorphic;

import com.sun.source.tree.ClassTree;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.Tree;
import com.sun.source.util.TreePath;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.netbeans.api.java.source.WorkingCopy;
import uno.anahata.asi.nb.tools.java.BatchCodeRefiner;

/**
 * Intent to delete an existing structural member.
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Schema(description = "Instruction to delete a structural member.")
public class DeleteMemberIntent extends CodeRefinementIntentPolymorphic {

    /** The target member FQN. */
    @Schema(description = "The ABSOLUTE FQN of the member to delete.", required = true)
    private String memberFqn;

    /**
     * {@inheritDoc}
     * <p>Implementation logic: Identifies the target member, resolves its 
     * position in the Surgery Table, and removes it from the parent container's 
     * member list.</p>
     */
    @Override
    public void apply(WorkingCopy wc, Map<Tree, List<Tree>> modifiedMembers, boolean optimize) throws Exception {
        Tree memberTree = BatchCodeRefiner.findMemberInWorkingCopy(wc, memberFqn);
        if (memberTree == null) {
            BatchCodeRefiner.throwMemberNotFound(wc, memberFqn);
        }
        TreePath path = TreePath.getPath(wc.getCompilationUnit(), memberTree);
        Tree parent = path.getParentPath().getLeaf();

        List<Tree> members = modifiedMembers.computeIfAbsent(parent, p -> {
            if (p instanceof ClassTree ct) {
                return new ArrayList<>(ct.getMembers());
            } else {
                return new ArrayList<>(((CompilationUnitTree) p).getTypeDecls());
            }
        });

        int idx = CodeRefinementBatchPolymorphic.findMemberIndex(wc, members, memberTree);
        if (idx != -1) {
            members.remove(idx);
        }
    }

    @Override
    public String getHtmlDisplay() {
        return "<font color='#F44336'>[-]</font> <b>Delete</b> " + getSimpleName(memberFqn);
    }
}
