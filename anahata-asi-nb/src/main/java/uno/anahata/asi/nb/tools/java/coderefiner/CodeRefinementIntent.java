/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import com.sun.source.tree.*;
import com.sun.source.util.TreePath;
import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.netbeans.api.java.source.GeneratorUtilities;
import org.netbeans.api.java.source.TreeMaker;
import org.netbeans.api.java.source.WorkingCopy;
import uno.anahata.asi.agi.tool.AgiToolException;

/**
 * A robust, flattened version of structural Java refinement intents.
 * <p>
 * This "Tagged Union" DTO replaces the polymorphic hierarchy to maximize 
 * compatibility with LLMs that struggle with 'oneOf' schemas.
 * </p>
 * 
 * @author anahata
 */
@Data
@NoArgsConstructor
@Schema(description = "Represents a single structural AST modification instruction in a flattened format.")
public class CodeRefinementIntent implements Serializable {

    /**
     * The type of structural operation to perform.
     */
    public enum Type {
        /** Inserts a new member. */
        @Schema(description = "Inserts a new member (method, field, or inner type).")
        INSERT,
        /** Updates an existing member's signature or body. */
        @Schema(description = "Updates an existing member's signature or body.")
        UPDATE,
        /** Deletes an existing member. */
        @Schema(description = "Deletes an existing member.")
        DELETE,
        /** Moves an existing member to a new position. */
        @Schema(description = "Moves an existing member to a new position within its class.")
        MOVE
    }

    @Schema(description = "The operation type.", required = true)
    private Type type;

    @Schema(description = "The FQN of the target class (e.g. 'com.foo.Bar'). Mandatory for 'insert' inside a class. Use '$' for nested types. Leave empty for file-level.")
    private String classFqn;

    @Schema(description = "The ABSOLUTE FQN of the member to operation on (e.g. 'com.foo.Bar.myMethod()'). Mandatory for 'update', 'delete', and 'move'.")
    private String memberFqn;

    @Schema(description = "The member signature (e.g. 'public void foo()'). Mandatory for 'insert', optional for 'update'.")
    private String declaration;

    @Schema(description = "The WHOLE body code. For methods, logic inside braces. For fields, the initializer expression (part after '='). Optional for 'insert' and 'update'.")
    private String body;

    @Schema(description = "Position relative to the anchor member. Mandatory for 'insert' and 'move'.")
    private RelativePosition position;

    @Schema(description = "Anchor member name relative to class (e.g. 'myMethod()'). Mandatory for BEFORE/AFTER positions in 'insert' and 'move'.")
    private String anchorMemberName;

    @Schema(description = "The reason for this structural change. Will be displayed in the UI.")
    private String reason;

    /**
     * Authoritatively applies this intent to the provided working copy.
     * <p>
     * Implementation Logic Parity: This method merges the logic from all 
     * legacy concrete intent classes into a single, switch-based dispatcher 
     * to ensure 100% functional consistency.
     * </p>
     */
    public void apply(WorkingCopy wc, Map<Tree, List<Tree>> modifiedMembers, boolean optimize) throws Exception {
        if (type == null) {
            throw new AgiToolException("Intent type is mandatory.");
        }

        switch (type) {
            case INSERT -> applyInsert(wc, modifiedMembers, optimize);
            case UPDATE -> applyUpdate(wc, modifiedMembers, optimize);
            case DELETE -> applyDelete(wc, modifiedMembers, optimize);
            case MOVE -> applyMove(wc, modifiedMembers, optimize);
        }
    }

    private void applyInsert(WorkingCopy wc, Map<Tree, List<Tree>> modifiedMembers, boolean optimize) throws Exception {
        GeneratorUtilities gu = GeneratorUtilities.get(wc);
        Tree newMember = CodeRefinementBatchPolymorphic.parseMember(wc, declaration, body);
        if (optimize) {
            newMember = gu.importFQNs(newMember);
        }

        Tree parentTree;
        if (classFqn == null || classFqn.isBlank()) {
            parentTree = wc.getCompilationUnit();
        } else {
            parentTree = CodeRefinementBatchPolymorphic.findMemberInWorkingCopy(wc, classFqn);
            if (parentTree == null) {
                throw new AgiToolException("Target class not found in current source: " + classFqn);
            }
        }

        List<Tree> members = getContainerMembers(parentTree, modifiedMembers);
        int insertIdx = CodeRefinementBatchPolymorphic.getInsertIndex(wc, members, position, anchorMemberName);
        members.add(insertIdx, newMember);
    }

    private void applyUpdate(WorkingCopy wc, Map<Tree, List<Tree>> modifiedMembers, boolean optimize) throws Exception {
        TreeMaker make = wc.getTreeMaker();
        GeneratorUtilities gu = GeneratorUtilities.get(wc);
        Tree oldTree = CodeRefinementBatchPolymorphic.findMemberInWorkingCopy(wc, memberFqn);

        if (oldTree == null) {
            CodeRefinementBatchPolymorphic.throwMemberNotFound(wc, memberFqn);
        }

        TreePath path = TreePath.getPath(wc.getCompilationUnit(), oldTree);
        Tree parent = path.getParentPath().getLeaf();
        List<Tree> members = getContainerMembers(parent, modifiedMembers);

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

            // Re-resolve positions after potential CU mutation
            Tree latestOldTree = CodeRefinementBatchPolymorphic.findMemberInWorkingCopy(wc, memberFqn);
            TreePath latestPath = TreePath.getPath(wc.getCompilationUnit(), latestOldTree);
            Tree latestParent = latestPath.getParentPath().getLeaf();
            List<Tree> latestMembers = getContainerMembers(latestParent, modifiedMembers);

            int idx = CodeRefinementBatchPolymorphic.findMemberIndex(wc, latestMembers, latestOldTree);
            if (idx != -1) {
                latestMembers.set(idx, make.asReplacementOf(newTree, latestMembers.get(idx)));
            }
        }
    }

    private void applyDelete(WorkingCopy wc, Map<Tree, List<Tree>> modifiedMembers, boolean optimize) throws Exception {
        Tree memberTree = CodeRefinementBatchPolymorphic.findMemberInWorkingCopy(wc, memberFqn);
        if (memberTree == null) {
            CodeRefinementBatchPolymorphic.throwMemberNotFound(wc, memberFqn);
        }
        TreePath path = TreePath.getPath(wc.getCompilationUnit(), memberTree);
        Tree parent = path.getParentPath().getLeaf();

        List<Tree> members = getContainerMembers(parent, modifiedMembers);
        int idx = CodeRefinementBatchPolymorphic.findMemberIndex(wc, members, memberTree);
        if (idx != -1) {
            members.remove(idx);
        }
    }

    private void applyMove(WorkingCopy wc, Map<Tree, List<Tree>> modifiedMembers, boolean optimize) throws Exception {
        Tree memberTree = CodeRefinementBatchPolymorphic.findMemberInWorkingCopy(wc, memberFqn);
        if (memberTree == null) {
            CodeRefinementBatchPolymorphic.throwMemberNotFound(wc, memberFqn);
        }
        TreePath path = TreePath.getPath(wc.getCompilationUnit(), memberTree);
        if (!(path.getParentPath().getLeaf() instanceof ClassTree ct)) {
            throw new AgiToolException("Only members of a class can be moved.");
        }

        List<Tree> members = getContainerMembers(ct, modifiedMembers);
        int idx = CodeRefinementBatchPolymorphic.findMemberIndex(wc, members, memberTree);
        if (idx != -1) {
            members.remove(idx);
        }
        int insertIdx = CodeRefinementBatchPolymorphic.getInsertIndex(wc, members, position, anchorMemberName);
        members.add(insertIdx, memberTree);
    }

    private List<Tree> getContainerMembers(Tree container, Map<Tree, List<Tree>> modifiedMembers) {
        return modifiedMembers.computeIfAbsent(container, p -> {
            if (p instanceof ClassTree ct) {
                return new ArrayList<>(ct.getMembers());
            } else {
                return new ArrayList<>(((CompilationUnitTree) p).getTypeDecls());
            }
        });
    }

    /**
     * Returns a human-readable HTML description of the intent for display in the UI.
     */
    public String getHtmlDisplay() {
        String color = switch(type) {
            case INSERT -> "#4CAF50";
            case UPDATE -> "#2196F3";
            case DELETE -> "#F44336";
            case MOVE -> "#FF9800";
        };
        String icon = switch(type) {
            case INSERT -> "[+]";
            case UPDATE -> "[*]";
            case DELETE -> "[-]";
            case MOVE -> "[M]";
        };
        
        String targetName = (memberFqn != null) ? getSimpleName(memberFqn) : "New Member";
        if (type == Type.INSERT && declaration != null) {
             targetName = getSimpleNameFromDeclaration(declaration);
        }

        StringBuilder sb = new StringBuilder("<font color='").append(color).append("'>").append(icon).append("</font> ");
        sb.append("<b>").append(type.toString().toUpperCase()).append("</b> <code>").append(targetName).append("</code>");
        
        if (position != null) {
            sb.append(" ").append(position);
            if (anchorMemberName != null) {
                sb.append(" ").append(getSimpleName(anchorMemberName));
            }
        }
        
        if (reason != null && !reason.isBlank()) {
            sb.append(" <i style='color: #888888;'>(").append(reason).append(")</i>");
        }
        
        return sb.toString();
    }

    /**
     * Determines the FQN that the target member will have after this intent is applied.
     * For inserts, it derives the name from the declaration.
     * 
     * @return The absolute FQN or null if it cannot be determined (e.g. DELETE).
     */
    public String getResultingMemberFqn() {
        if (type == Type.DELETE) return null;
        if (type == Type.INSERT) {
            String name = getSimpleNameFromDeclaration(declaration);
            if (classFqn == null || classFqn.isBlank()) return name;
            return classFqn + "." + name;
        }
        return memberFqn;
    }

    private String getSimpleName(String fqn) {
        if (fqn == null || fqn.isBlank()) return "Unknown";
        int paren = fqn.indexOf('(');
        String namePart = (paren == -1) ? fqn : fqn.substring(0, paren);
        int lastDot = Math.max(namePart.lastIndexOf('.'), namePart.lastIndexOf('$'));
        return (lastDot == -1) ? namePart : namePart.substring(lastDot + 1);
    }

    private String getSimpleNameFromDeclaration(String decl) {
        String clean = decl.trim();
        int paren = clean.indexOf('(');
        int end = (paren != -1) ? paren : (clean.endsWith(";") ? clean.length() - 1 : clean.length());
        int start = clean.lastIndexOf(' ', end - 1);
        String name = clean.substring(start + 1, end).trim();
        return (paren != -1) ? name + "()" : name;
    }
}
