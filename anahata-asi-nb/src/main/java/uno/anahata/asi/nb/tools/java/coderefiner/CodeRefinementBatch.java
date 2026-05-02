/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import com.sun.source.tree.*;
import com.sun.source.util.SourcePositions;
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

    @Schema(description = "The list of structural changes to apply, in order. Note the intents will be mapped to one of the listed java types", required = true)
    private List<CodeRefinementIntent> intents = new ArrayList<>();

    @Schema(description = "Whether to optimize imports after applying all changes. Defaults to true.")
    private boolean optimize = true;

    @Schema(description = "Whether to save the file to disk after refinement. Defaults to true.")
    private boolean save = true;

    /**
     * {@inheritDoc}
     * <p>
     * Calculates the resulting source code by simulating the AST modifications
     * defined in the batch. This is used by the UI to show a unified
     * side-by-side diff before execution.</p>
     */
    @Override
    protected String doCalculateResultingContent(Agi agi) throws Exception {
        // 1. Authoritative state capture (if not already done by validate)
        if (originalContent == null) {
             captureOriginalContent(agi);
        }
        
        uno.anahata.asi.agi.resource.Resource res = agi.getResourceManager().get(resourceUuid);
        if (res == null) {
            throw new AgiToolException("no resource found for uuid " + resourceUuid);
        }
        
        if (!(res.getHandle() instanceof uno.anahata.asi.nb.resources.handle.NbHandle nbh)) {
            throw new AgiToolException("Resource handle is not a NbHandle " + res.getHandle());
        }

        FileObject realFo = nbh.getFileObject();
        log.info("Calculating resulting content for: {}", realFo);

        // 2. Perform sequential AST replay directly on the real FileObject to preserve Classpath/Project context.
        JavaSource js = JavaSource.forFileObject(realFo);
        ModificationResult mRes = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            applyTo(wc);
        });

        // 3. Extract the resulting source string
        String ret = mRes.getResultingSource(realFo);
        return ret;
    }

    /**
     * Authoritatively applies all intents in this batch to the provided working
     * copy using a single-shot atomic rewrite strategy.
     *
     * @param wc The working copy to modify.
     * @throws Exception if any intent application fails.
     */
    public void applyTo(WorkingCopy wc) throws Exception {
        log.info("[V3-STRATEGY] Starting batch application. Intents: {}", intents.size());
        
        // Accumulate changes by parent type to ensure atomic rewrites
        Map<Tree, List<Tree>> modifiedMembers = new LinkedHashMap<>();

        for (CodeRefinementIntent intent : intents) {
            log.info("Processing intent: {}", intent.getClass().getSimpleName());
            intent.apply(wc, modifiedMembers, optimize);
        }

        // Perform single-shot atomic rewrites for each modified container
        TreeMaker make = wc.getTreeMaker();
        for (Map.Entry<Tree, List<Tree>> entry : modifiedMembers.entrySet()) {
            Tree parent = entry.getKey();
            List<Tree> members = entry.getValue();

            if (parent instanceof ClassTree ct) {
                log.info("Commiting rewrite for ClassTree: {} (Members: {})", ct.getSimpleName(), members.size());
                wc.rewrite(ct, rebuildClassTree(make, ct, members));
            } else if (parent instanceof CompilationUnitTree cut) {
                log.info("Commiting rewrite for CompilationUnitTree");
                CompilationUnitTree updated = make.CompilationUnit(cut.getPackage(), cut.getImports(), (List<? extends Tree>) members, cut.getSourceFile());
                wc.rewrite(cut, updated);
            }
        }
    }

    /**
     * Internal utility to find a member in the working copy context.
     * 
     * @param wc WorkingCopy
     * @param memberFqn Member FQN
     * @return The leaf Tree node or null.
     */
    public static Tree findMemberInWorkingCopy(WorkingCopy wc, String memberFqn) {
        Tree found = JavaSourceUtils.findTree(wc, memberFqn);
        if (found == null) {
            return null;
        }
        TreePath path = TreePath.getPath(wc.getCompilationUnit(), found);
        return path != null ? path.getLeaf() : null;
    }

    /**
     * Internal utility to find the index of a specific tree node within a list 
     * of members based on source positions.
     */
    public static int findMemberIndex(WorkingCopy wc, List<? extends Tree> members, Tree target) {
        if (wc == null || members == null || target == null) {
            return -1;
        }
        SourcePositions sp = wc.getTrees().getSourcePositions();
        CompilationUnitTree cut = wc.getCompilationUnit();
        long targetStart = sp.getStartPosition(cut, target);
        for (int i = 0; i < members.size(); i++) {
            if (sp.getStartPosition(cut, members.get(i)) == targetStart) {
                return i;
            }
        }
        return -1;
    }
    
    /**
     * Finds the index of a member by its name or canonical signature.
     *
     * @param wc The working copy for resolution.
     * @param members The list of class members.
     * @param memberName The name or signature to look for.
     * @return The index, or -1 if not found.
     */
    private static int findMemberIndex(WorkingCopy wc, List<? extends Tree> members, String memberName) {
        for (int i = 0; i < members.size(); i++) {
            Tree m = members.get(i);
            String name = null;
            String signature = null;
            if (m instanceof MethodTree mt) {
                name = mt.getName().toString();
                if (wc != null) {
                    TreePath path = TreePath.getPath(wc.getCompilationUnit(), m);
                    Element e = wc.getTrees().getElement(path);
                    if (e instanceof ExecutableElement ee) {
                        String params = ee.getParameters().stream().map(p -> {
                            String t = p.asType().toString();
                            int bracket = t.indexOf('<');
                            return bracket != -1 ? t.substring(0, bracket) : t;
                        }).collect(Collectors.joining(","));
                        signature = (name.equals("<init>") ? "<init>" : name) + "(" + params + ")";
                    }
                }
            } else if (m instanceof VariableTree vt) {
                name = vt.getName().toString();
            } else if (m instanceof ClassTree ct) {
                name = ct.getSimpleName().toString();
            } else if (m.getKind() == Tree.Kind.BLOCK) {
                name = ((BlockTree) m).isStatic() ? "<clinit>" : "<init-block>";
            }
            if (memberName.equals(name) || memberName.equals(signature)) {
                return i;
            }
            if (memberName.contains("#")) {
                String typePart = memberName.substring(0, memberName.indexOf('#'));
                int targetIndex = Integer.parseInt(memberName.substring(memberName.indexOf('#') + 1, memberName.indexOf('(')));
                int currentCount = 0;
                for (Tree prev : members) {
                    String prevName = null;
                    if (prev.getKind() == Tree.Kind.BLOCK) {
                        prevName = ((BlockTree) prev).isStatic() ? "<clinit>" : "<init-block>";
                    }
                    if (typePart.equals(prevName)) {
                        if (++currentCount == targetIndex && prev == m) {
                            return i;
                        }
                    }
                }
            }
        }
        return -1;
    }

    /**
     * Internal utility to parse a member declaration and optional body.
     */
    public static Tree parseMember(WorkingCopy wc, String declaration, String body) throws Exception {
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
                Tree t = null;
                if (isStandaloneType) {
                    t = cut.getTypeDecls().get(0);
                } else {
                    ClassTree ct = (ClassTree) cut.getTypeDecls().get(0);
                    for (Tree member : ct.getMembers()) {
                        if (member instanceof MethodTree mt && mt.getName().contentEquals("<init>") && !finalDecl.contains("<init>")) {
                            if (ct.getMembers().size() > 1) {
                                continue;
                            }
                        }
                        t = member;
                        break;
                    }
                }
                if (t != null) {
                    result[0] = wc.getTreeMaker().asNew(t);
                }
            }
        }, true);
        return result[0];
    }

    /**
     * Resolves the insertion index relative to anchors.
     */
    public static int getInsertIndex(WorkingCopy wc, List<? extends Tree> members, RelativePosition position, String anchor) throws AgiToolException {
        int anchorIdx = anchor != null ? findMemberIndex(wc, members, getMemberSignature(anchor)) : -1;
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
    
    /**
     * Extracts the member signature (name + parameters) from an FQN.
     * Unlike getMemberSimpleName, this preserves the parameter list for methods.
     *
     * @param memberFqn The FQN to parse (e.g. 'com.foo.Bar.method(int)').
     * @return The signature part (e.g. 'method(int)').
     */
    public static String getMemberSignature(String memberFqn) {
        if (memberFqn == null || memberFqn.isBlank()) {
            return memberFqn;
        }
        int paren = memberFqn.indexOf('(');
        String namePart = paren != -1 ? memberFqn.substring(0, paren) : memberFqn;
        int lastSeparator = Math.max(namePart.lastIndexOf('.'), namePart.lastIndexOf('$'));
        return memberFqn.substring(lastSeparator + 1);
    }

    /**
     * Rebuilds a ClassTree container with a new list of members.
     */
    public static ClassTree rebuildClassTree(TreeMaker make, ClassTree ct, List<Tree> members) {
        return switch (ct.getKind()) {
            case INTERFACE ->
                make.Interface(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(), (List<ExpressionTree>) (List<?>) ct.getImplementsClause(), (List<ExpressionTree>) (List<?>) ct.getPermitsClause(), members);
            case ENUM ->
                make.Enum(ct.getModifiers(), ct.getSimpleName(), (List<ExpressionTree>) (List<?>) ct.getImplementsClause(), members);
            case ANNOTATION_TYPE ->
                make.AnnotationType(ct.getModifiers(), ct.getSimpleName(), members);
            case RECORD ->
                // NOTE: NetBeans TreeMaker lacks make.Record. Using Class with bit 61 is the current workaround.
                make.Class(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(), null, (List<ExpressionTree>) (List<?>) ct.getImplementsClause(), (List<ExpressionTree>) (List<?>) ct.getPermitsClause(), members);
            default ->
                make.Class(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(), ct.getExtendsClause(), (List<ExpressionTree>) (List<?>) ct.getImplementsClause(), (List<ExpressionTree>) (List<?>) ct.getPermitsClause(), members);
        };
    }

    /**
     * Clones a tree node into the current WorkingCopy context.
     */
    public static Tree cloneTree(TreeMaker make, Tree tree) {
        if (tree instanceof ClassTree ct) {
            return rebuildClassTree(make, ct, new ArrayList<>(ct.getMembers()));
        } else if (tree instanceof MethodTree mt) {
            return make.Method(mt.getModifiers(), mt.getName(), mt.getReturnType(), mt.getTypeParameters(), mt.getParameters(), mt.getThrows(), mt.getBody(), (AnnotationTree) mt.getDefaultValue());
        } else if (tree instanceof VariableTree vt) {
            return make.Variable(vt.getModifiers(), vt.getName(), vt.getType(), vt.getInitializer());
        }
        return tree;
    }

    /**
     * Throws a detailed exception if a member is not found, providing candidate suggestions.
     */
    public static void throwMemberNotFound(WorkingCopy wc, String memberFqn) throws AgiToolException {
        int paren = memberFqn.indexOf("(");
        String namePart = paren != -1 ? memberFqn.substring(0, paren) : memberFqn;
        int lastSeparator = Math.max(namePart.lastIndexOf("."), namePart.lastIndexOf("$"));
        if (lastSeparator == -1) {
            throw new AgiToolException("Member not found: " + memberFqn);
        }

        String parentFqn = namePart.substring(0, lastSeparator);
        String name = namePart.substring(lastSeparator + 1);
        TypeElement parent = wc.getElements().getTypeElement(JavaSourceUtils.normalizeFqn(parentFqn));
        if (parent == null) {
            throw new AgiToolException("Member not found: " + memberFqn + " (Parent class not found: " + parentFqn + ")");
        }

        List<String> candidates = new ArrayList<>();
        for (Element e : parent.getEnclosedElements()) {
            if (e.getSimpleName().contentEquals(name)) {
                if (e instanceof ExecutableElement ee) {
                    String params = ee.getParameters().stream()
                            .map(p -> p.asType().toString().replaceAll("<.*>", ""))
                            .collect(Collectors.joining(","));
                    candidates.add(parentFqn + "." + (e.getKind() == javax.lang.model.element.ElementKind.CONSTRUCTOR ? "<init>" : name) + "(" + params + ")");
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
     * <p>
     * Validates that the resource is in context and is a valid Java source.</p>
     */
    @Override
    public void validate(Agi agi) throws Exception {
        validateStructuralState(agi);
        if (intents == null || intents.isEmpty()) {
            throw new AgiToolException("Refinement batch must contain at least one intent.");
        }
        validateIdenticalContent(agi);
    }
}
