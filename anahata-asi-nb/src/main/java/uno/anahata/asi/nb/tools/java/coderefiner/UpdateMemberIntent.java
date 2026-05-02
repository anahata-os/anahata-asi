/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import com.sun.source.tree.AnnotationTree;
import com.sun.source.tree.ClassTree;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.ExpressionTree;
import com.sun.source.tree.MethodTree;
import com.sun.source.tree.Tree;
import com.sun.source.tree.VariableTree;
import com.sun.source.util.TreePath;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.netbeans.api.java.source.GeneratorUtilities;
import org.netbeans.api.java.source.TreeMaker;
import org.netbeans.api.java.source.WorkingCopy;

/**
 * Intent to update an existing structural member.
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Schema(description = "Instruction to update an existing structural member.")
public class UpdateMemberIntent extends CodeRefinementIntentPolymorphic {

    @Schema(description = "The ABSOLUTE FQN of the member to update (e.g. 'com.foo.Bar.myMethod()').", required = true)
    private String memberFqn;

    @Schema(description = "The new member declaration (signature). If omitted, the existing signature is preserved.")
    private String declaration;

    @Schema(description = "The new WHOLE body code. For methods, logic inside braces. For fields, the initializer expression (part after '='). If omitted, existing body is preserved.")
    private String body;

    /**
     * {@inheritDoc}
     * <p>Implementation logic: Resolves the existing member in the WorkingCopy, 
     * captures its parent container, and replaces it in the Surgery Table with 
     * a rebuilt tree reflecting signature or body changes.</p>
     */
    @Override
    public void apply(WorkingCopy wc, Map<Tree, List<Tree>> modifiedMembers, boolean optimize) throws Exception {
        TreeMaker make = wc.getTreeMaker();
        GeneratorUtilities gu = GeneratorUtilities.get(wc);
        Tree oldTree = CodeRefinementBatchPolymorphic.findMemberInWorkingCopy(wc, memberFqn);

        if (oldTree == null) {
            CodeRefinementBatchPolymorphic.throwMemberNotFound(wc, memberFqn);
        }

        TreePath path = TreePath.getPath(wc.getCompilationUnit(), oldTree);
        Tree parent = path.getParentPath().getLeaf();
        List<Tree> members = modifiedMembers.computeIfAbsent(parent, p -> {
            if (p instanceof ClassTree ct) {
                return new ArrayList<>(ct.getMembers());
            } else {
                return new ArrayList<>(((CompilationUnitTree) p).getTypeDecls());
            }
        });

        if (declaration != null || body != null) {
            Tree newTree;
            if (declaration == null) {
                newTree = CodeRefinementBatchPolymorphic.cloneTree(make, oldTree);
                if (body != null) {
                    if (newTree instanceof MethodTree mt) {
                        String wrappedBody = body.trim().startsWith("{") ? body : "{" + body + "\n}";
                        newTree = make.Method(mt.getModifiers(), mt.getName(), mt.getReturnType(), mt.getTypeParameters(), mt.getParameters(), mt.getThrows(), make.createMethodBody((MethodTree) oldTree, wrappedBody), (AnnotationTree) mt.getDefaultValue());
                    } else if (newTree instanceof VariableTree vt) {
                        ExpressionTree finalInit = wc.getTreeUtilities().parseExpression(body, null);
                        newTree = make.Variable(vt.getModifiers(), vt.getName(), vt.getType(), finalInit);
                    }
                }
            } else {
                newTree = CodeRefinementBatchPolymorphic.parseMember(wc, declaration, body);
                if (body == null) {
                    if (oldTree instanceof MethodTree oldMt && newTree instanceof MethodTree newMt) {
                        newTree = make.Method(newMt.getModifiers(), newMt.getName(), newMt.getReturnType(), newMt.getTypeParameters(), newMt.getParameters(), newMt.getThrows(), oldMt.getBody(), (AnnotationTree) newMt.getDefaultValue());
                    } else if (oldTree instanceof VariableTree oldVt && newTree instanceof VariableTree newVt) {
                        newTree = make.Variable(newVt.getModifiers(), newVt.getName(), newVt.getType(), oldVt.getInitializer());
                    }
                }
            }
            
            if (optimize) {
                newTree = gu.importFQNs(newTree);
            }
            
            gu.copyComments(oldTree, newTree, true);
            if (body == null) {
                gu.copyComments(oldTree, newTree, false);
            }

            // RE-RESOLVE parent and oldTree index after potential CU mutation
            Tree latestOldTree = CodeRefinementBatchPolymorphic.findMemberInWorkingCopy(wc, memberFqn);
            TreePath latestPath = TreePath.getPath(wc.getCompilationUnit(), latestOldTree);
            Tree latestParent = latestPath.getParentPath().getLeaf();
            
            members = modifiedMembers.computeIfAbsent(latestParent, p -> {
                if (p instanceof ClassTree ct) {
                    return new ArrayList<>(ct.getMembers());
                } else {
                    return new ArrayList<>(((CompilationUnitTree) p).getTypeDecls());
                }
            });

            int idx = CodeRefinementBatchPolymorphic.findMemberIndex(wc, members, latestOldTree);
            if (idx != -1) {
                members.set(idx, make.asReplacementOf(newTree, members.get(idx)));
            }
        }
    }

    @Override
    public String getHtmlDisplay() {
        return "<font color='#2196F3'>[*]</font> <b>Update</b> " + getSimpleName(memberFqn);
    }
}
