/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import com.sun.source.tree.*;
import com.sun.source.util.TreePath;
import io.swagger.v3.oas.annotations.media.Schema;
import java.io.OutputStream;
import java.util.*;
import java.util.stream.Collectors;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.source.*;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.nb.tools.java.JavaSourceUtils;
import uno.anahata.asi.nb.tools.java.JavaSourceUtils.RelativePosition;
import uno.anahata.asi.toolkit.resources.text.AbstractTextResourceWrite;

/**
 * A container for a batch of structural Java refinement operations.
 * <p>
 * This class orchestrates multiple {@link CodeRefinementIntent}s targeted at a 
 * single source file. It extends {@link AbstractTextResourceWrite} to leverage 
 * the platform's high-fidelity diff rendering and resource-locking mechanisms.
 * </p>
 * 
 * @author anahata
 */
@Data
@Slf4j
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Schema(description = "A batch of structural AST modifications for a single Java file.")
public class CodeRefinementBatch extends AbstractTextResourceWrite {

    @Schema(description = "A manual override of the resulting content. If provided, AST intents are ignored.")
    private String manualOverride;

    @Schema(description = "The list of structural changes to apply, in order.", required = true)
    private List<CodeRefinementIntent> intents = new ArrayList<>();

    @Schema(description = "Whether to optimize imports after applying all changes. Defaults to true.")
    private boolean optimize = true;

    @Schema(description = "Whether to save the file to disk after refinement. Defaults to true.")
    private boolean save = true;

    /**
     * {@inheritDoc} 
     * <p>Calculates the resulting source code by simulating the AST modifications 
     * defined in the batch. This is used by the UI to show a unified side-by-side 
     * diff before execution.</p>
     */
    @Override
    public String calculateResultingContent() throws Exception {
        if (originalContent == null) {
            return null;
        }

        // 1. Create a memory-backed FileObject for the simulation
        FileObject tempFo = FileUtil.createMemoryFileSystem().getRoot().createData("Simulation", "java");
        try (OutputStream os = tempFo.getOutputStream()) {
            os.write(originalContent.getBytes());
        }

        // 2. Perform sequential AST replay
        JavaSource js = JavaSource.forFileObject(tempFo);
        ModificationResult res = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            applyTo(wc);
        });

        // 3. Extract the resulting source without touching the real disk
        return res.getResultingSource(tempFo);
    }

    /**
     * Authoritatively applies all intents in this batch to the provided working copy.
     * 
     * @param wc The working copy to modify.
     * @throws Exception if any intent application fails.
     */
    public void applyTo(WorkingCopy wc) throws Exception {
        for (CodeRefinementIntent intent : intents) {
            applyIntent(wc, intent);
        }
        if (optimize) {
            CompilationUnitTree cut = GeneratorUtilities.get(wc).importFQNs(wc.getCompilationUnit());
            wc.rewrite(wc.getCompilationUnit(), cut);
        }
    }

    /**
     * Internal dispatcher that applies a single intent to the working copy.
     */
    private void applyIntent(WorkingCopy wc, CodeRefinementIntent intent) throws Exception {
        if (intent instanceof InsertMemberIntent ins) {
            applyInsert(wc, ins);
        } else if (intent instanceof UpdateMemberIntent upd) {
            applyUpdate(wc, upd);
        } else if (intent instanceof DeleteMemberIntent del) {
            applyDelete(wc, del);
        } else if (intent instanceof MoveMemberIntent mov) {
            applyMove(wc, mov);
        }
    }

    private void applyInsert(WorkingCopy wc, InsertMemberIntent intent) throws Exception {
        TreeMaker make = wc.getTreeMaker();
        Tree parentTree;
        List<Tree> members;
        String classFqn = intent.getClassFqn();

        if (classFqn == null || classFqn.isBlank()) {
            parentTree = wc.getCompilationUnit();
            members = new ArrayList<>(((CompilationUnitTree) parentTree).getTypeDecls());
        } else {
            Element resolved = JavaSourceUtils.findElement(wc, classFqn);
            if (!(resolved instanceof TypeElement te)) {
                throw new AgiToolException("Target class not found: " + classFqn);
            }
            parentTree = wc.getTrees().getTree(te);
            members = new ArrayList<>(((ClassTree) parentTree).getMembers());
        }

        Tree newMember = parseMember(wc, intent.getDeclaration(), intent.getBody());
        int insertIdx = getInsertIndex(wc, members, intent.getPosition(), intent.getAnchorMemberName());
        members.add(insertIdx, newMember);

        if (parentTree instanceof ClassTree ct) {
            wc.rewrite(ct, rebuildClassTree(make, ct, members));
        } else if (parentTree instanceof CompilationUnitTree cut) {
            CompilationUnitTree updated = make.CompilationUnit(cut.getPackage(), cut.getImports(), (List<Tree>) (List<?>) members, cut.getSourceFile());
            wc.rewrite(cut, updated);
        }
    }

    private void applyUpdate(WorkingCopy wc, UpdateMemberIntent intent) throws Exception {
        TreeMaker make = wc.getTreeMaker();
        GeneratorUtilities gu = GeneratorUtilities.get(wc);
        Tree oldTree = JavaSourceUtils.findTree(wc, intent.getMemberFqn());
        
        if (oldTree == null) {
            throwMemberNotFound(wc, intent.getMemberFqn());
        }

        String dec = intent.getDeclaration();
        String body = intent.getBody();

        if (dec != null || body != null) {
            Tree newTree;
            if (dec == null) {
                newTree = cloneTree(make, oldTree);
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
                newTree = parseMember(wc, dec, body);
                if (body == null) {
                    if (oldTree instanceof MethodTree oldMt && newTree instanceof MethodTree newMt) {
                        newTree = make.Method(newMt.getModifiers(), newMt.getName(), newMt.getReturnType(), newMt.getTypeParameters(), newMt.getParameters(), newMt.getThrows(), oldMt.getBody(), (AnnotationTree) newMt.getDefaultValue());
                    } else if (oldTree instanceof VariableTree oldVt && newTree instanceof VariableTree newVt) {
                        newTree = make.Variable(newVt.getModifiers(), newVt.getName(), newVt.getType(), oldVt.getInitializer());
                    }
                }
            }
            gu.copyComments(oldTree, newTree, true);
            if (body == null) {
                gu.copyComments(oldTree, newTree, false);
            }
            wc.rewrite(oldTree, make.asReplacementOf(newTree, oldTree));
        }
    }

    private void applyDelete(WorkingCopy wc, DeleteMemberIntent intent) throws AgiToolException {
        Tree memberTree = JavaSourceUtils.findTree(wc, intent.getMemberFqn());
        if (memberTree == null) {
            throwMemberNotFound(wc, intent.getMemberFqn());
        }
        TreePath path = TreePath.getPath(wc.getCompilationUnit(), memberTree);
        Tree parent = path.getParentPath().getLeaf();
        TreeMaker make = wc.getTreeMaker();

        if (parent instanceof ClassTree ct) {
            List<Tree> members = new ArrayList<>(ct.getMembers());
            if (members.remove(memberTree)) {
                wc.rewrite(ct, rebuildClassTree(make, ct, members));
            }
        } else if (parent instanceof CompilationUnitTree cut) {
            List<Tree> types = new ArrayList<>(cut.getTypeDecls());
            if (types.remove(memberTree)) {
                CompilationUnitTree updated = make.CompilationUnit(cut.getPackage(), cut.getImports(), types, cut.getSourceFile());
                wc.rewrite(cut, updated);
            }
        }
    }

    private void applyMove(WorkingCopy wc, MoveMemberIntent intent) throws Exception {
        TreeMaker make = wc.getTreeMaker();
        Tree memberTree = JavaSourceUtils.findTree(wc, intent.getMemberFqn());
        if (memberTree == null) {
            throwMemberNotFound(wc, intent.getMemberFqn());
        }
        TreePath path = TreePath.getPath(wc.getCompilationUnit(), memberTree);
        if (!(path.getParentPath().getLeaf() instanceof ClassTree ct)) {
            throw new AgiToolException("Only members of a class can be moved.");
        }
        List<Tree> members = new ArrayList<>(ct.getMembers());
        members.remove(memberTree);
        int insertIdx = getInsertIndex(wc, members, intent.getPosition(), intent.getAnchorMemberName());
        members.add(insertIdx, memberTree);
        wc.rewrite(ct, make.asReplacementOf(rebuildClassTree(make, ct, members), ct));
    }

    // --- Inlined AST Helpers ---

    private static Tree parseMember(WorkingCopy wc, String declaration, String body) throws Exception {
        if (declaration == null || declaration.isBlank()) {
            throw new AgiToolException("Member declaration cannot be null or empty.");
        }
        String decl = declaration.trim();
        boolean isStandaloneType = decl.startsWith("record ") || decl.contains(" record ") || decl.startsWith("class ") || decl.contains(" class ") || decl.startsWith("interface ") || decl.contains(" interface ") || decl.startsWith("enum ") || decl.contains(" enum ");
        if (!decl.endsWith(";") && !decl.endsWith("}")) {
            if (decl.contains("(") || isStandaloneType) {
                String b = (body == null) ? "{}" : (body.trim().startsWith("{") ? body : "{" + body + "}");
                decl += " " + b;
            } else {
                decl += ";";
            }
        }
        final String finalDecl = decl;
        String dummyClassName = isStandaloneType ? "DummyType" : "__Dummy";
        FileObject tempFo = FileUtil.createMemoryFileSystem().getRoot().createData(dummyClassName, "java");
        String dummyCode = isStandaloneType ? finalDecl : "class " + dummyClassName + " { " + finalDecl + " }";
        try (OutputStream os = tempFo.getOutputStream()) {
            os.write(dummyCode.getBytes());
        }
        JavaSource js = JavaSource.forFileObject(tempFo);
        final Tree[] result = new Tree[1];
        js.runUserActionTask(innerWc -> {
            innerWc.toPhase(JavaSource.Phase.PARSED);
            CompilationUnitTree cut = innerWc.getCompilationUnit();
            if (!cut.getTypeDecls().isEmpty()) {
                if (isStandaloneType) {
                    result[0] = cut.getTypeDecls().get(0);
                } else {
                    ClassTree ct = (ClassTree) cut.getTypeDecls().get(0);
                    for (Tree member : ct.getMembers()) {
                        if (member instanceof MethodTree mt && mt.getName().contentEquals("<init>") && !finalDecl.contains("<init>")) {
                            if (ct.getMembers().size() > 1) continue;
                        }
                        result[0] = member;
                        break;
                    }
                }
            }
        }, true);
        return result[0];
    }

    private static int getInsertIndex(WorkingCopy wc, List<? extends Tree> members, RelativePosition position, String anchor) throws AgiToolException {
        int anchorIdx = anchor != null ? JavaSourceUtils.findMemberIndex(wc, members, anchor) : -1;
        if (anchor != null && anchorIdx == -1) {
            throw new AgiToolException("Anchor member not found: " + anchor);
        }
        return switch (position) {
            case START -> 0;
            case END -> members.size();
            case BEFORE -> anchorIdx;
            case AFTER -> anchorIdx + 1;
        };
    }

    private static ClassTree rebuildClassTree(TreeMaker make, ClassTree ct, List<Tree> members) {
        return switch (ct.getKind()) {
            case INTERFACE -> make.Interface(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(), (List<ExpressionTree>)(List<?>)ct.getImplementsClause(), (List<ExpressionTree>)(List<?>)ct.getPermitsClause(), members);
            case ENUM -> make.Enum(ct.getModifiers(), ct.getSimpleName(), (List<ExpressionTree>)(List<?>)ct.getImplementsClause(), members);
            case ANNOTATION_TYPE -> make.AnnotationType(ct.getModifiers(), ct.getSimpleName(), members);
            case RECORD -> make.Class(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(), null, (List<ExpressionTree>)(List<?>)ct.getImplementsClause(), (List<ExpressionTree>)(List<?>)ct.getPermitsClause(), members);
            default -> make.Class(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(), ct.getExtendsClause(), (List<ExpressionTree>)(List<?>)ct.getImplementsClause(), (List<ExpressionTree>)(List<?>)ct.getPermitsClause(), members);
        };
    }

    private static Tree cloneTree(TreeMaker make, Tree tree) {
        if (tree instanceof ClassTree ct) {
            return rebuildClassTree(make, ct, new ArrayList<>(ct.getMembers()));
        } else if (tree instanceof MethodTree mt) {
            return make.Method(mt.getModifiers(), mt.getName(), mt.getReturnType(), mt.getTypeParameters(), mt.getParameters(), mt.getThrows(), mt.getBody(), (AnnotationTree) mt.getDefaultValue());
        } else if (tree instanceof VariableTree vt) {
            return make.Variable(vt.getModifiers(), vt.getName(), vt.getType(), vt.getInitializer());
        }
        return tree;
    }

    private static void throwMemberNotFound(WorkingCopy wc, String memberFqn) throws AgiToolException {
        int paren = memberFqn.indexOf("(");
        String namePart = paren != -1 ? memberFqn.substring(0, paren) : memberFqn;
        int lastSeparator = Math.max(namePart.lastIndexOf("."), namePart.lastIndexOf("$"));
        if (lastSeparator == -1) throw new AgiToolException("Member not found: " + memberFqn);
        
        String parentFqn = namePart.substring(0, lastSeparator);
        String name = namePart.substring(lastSeparator + 1);
        TypeElement parent = wc.getElements().getTypeElement(parentFqn);
        if (parent == null) throw new AgiToolException("Member not found: " + memberFqn + " (Parent class not found: " + parentFqn + ")");
        
        List<String> candidates = new ArrayList<>();
        for (Element e : parent.getEnclosedElements()) {
            if (e.getSimpleName().contentEquals(name)) {
                if (e instanceof ExecutableElement ee) {
                    String params = ee.getParameters().stream()
                            .map(p -> p.asType().toString().replaceAll("<.*>", ""))
                            .collect(Collectors.joining(","));
                    candidates.add(parentFqn + "." + name + "(" + params + ")");
                } else {
                    candidates.add(parentFqn + "." + name);
                }
            }
        }
        StringBuilder sb = new StringBuilder("Member not found: ").append(memberFqn);
        if (!candidates.isEmpty()) {
            sb.append("\nDid you mean one of these canonical FQNs?\n");
            candidates.forEach(c -> sb.append("- ").append(c).append("\n"));
        }
        throw new AgiToolException(sb.toString());
    }

    /**
     * {@inheritDoc} 
     * <p>Validates that the resource is in context and is a valid Java source.</p>
     */
    @Override
    public void validate(Agi agi) throws Exception {
        super.validate(agi);
        if (intents == null || intents.isEmpty()) {
            throw new AgiToolException("Refinement batch must contain at least one intent.");
        }
    }
}
