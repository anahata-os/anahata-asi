/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import com.sun.source.tree.ClassTree;
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
import uno.anahata.asi.agi.tool.AgiToolException;

/**
 * Intent to move an existing member to a new position.
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Schema(description = "Instruction to move a member within its enclosing type.")
public class MoveMemberIntent extends CodeRefinementIntentPolymorphic {

    @Schema(description = "The ABSOLUTE FQN of the member to move.", required = true)
    private String memberFqn;

    @Schema(description = "The new position relative to the anchor.", required = true)
    private RelativePosition position;

    @Schema(description = "The anchor member name. Mandatory for BEFORE/AFTER.")
    private String anchorMemberName;

    /**
     * {@inheritDoc}
     * <p>Implementation logic: Detaches the member from its current position 
     * in the Surgery Table and re-inserts it relative to the anchor index.</p>
     */
    @Override
    public void apply(WorkingCopy wc, Map<Tree, List<Tree>> modifiedMembers, boolean optimize) throws Exception {
        Tree memberTree = CodeRefinementBatchPolymorphic.findMemberInWorkingCopy(wc, memberFqn);
        if (memberTree == null) {
            CodeRefinementBatchPolymorphic.throwMemberNotFound(wc, memberFqn);
        }
        TreePath path = TreePath.getPath(wc.getCompilationUnit(), memberTree);
        if (!(path.getParentPath().getLeaf() instanceof ClassTree ct)) {
            throw new AgiToolException("Only members of a class can be moved.");
        }

        List<Tree> members = modifiedMembers.computeIfAbsent(ct, p -> {
            return new ArrayList<>(((ClassTree) p).getMembers());
        });

        int idx = CodeRefinementBatchPolymorphic.findMemberIndex(wc, members, memberTree);
        if (idx != -1) {
            members.remove(idx);
        }
        int insertIdx = CodeRefinementBatchPolymorphic.getInsertIndex(wc, members, position, anchorMemberName);
        members.add(insertIdx, memberTree);
    }

    @Override
    public String getHtmlDisplay() {
        StringBuilder sb = new StringBuilder("<font color='#FF9800'>[M]</font> <b>Move</b> ").append(getSimpleName(memberFqn)).append(" ").append(position);
        if (position == uno.anahata.asi.nb.tools.java.coderefiner.RelativePosition.BEFORE || position == uno.anahata.asi.nb.tools.java.coderefiner.RelativePosition.AFTER) {
            sb.append(" ").append(anchorMemberName != null ? getSimpleName(anchorMemberName) : "null");
        }
        return sb.toString();
    }
}
