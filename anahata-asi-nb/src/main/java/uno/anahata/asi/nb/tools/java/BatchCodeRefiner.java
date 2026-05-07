/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java;

import com.sun.source.tree.AnnotationTree;
import com.sun.source.tree.BlockTree;
import com.sun.source.tree.ClassTree;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.ExpressionTree;
import com.sun.source.tree.MethodTree;
import com.sun.source.tree.Tree;
import static com.sun.source.tree.Tree.Kind.ANNOTATION_TYPE;
import static com.sun.source.tree.Tree.Kind.ENUM;
import static com.sun.source.tree.Tree.Kind.INTERFACE;
import static com.sun.source.tree.Tree.Kind.RECORD;
import com.sun.source.tree.VariableTree;
import com.sun.source.util.SourcePositions;
import com.sun.source.util.TreePath;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.source.ClasspathInfo;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.java.source.TreeMaker;
import org.netbeans.api.java.source.WorkingCopy;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.nb.resources.handle.NbHandle;
import uno.anahata.asi.nb.tools.java.coderefiner.CodeRefinementBatch;
import uno.anahata.asi.nb.tools.java.coderefiner.RelativePosition;
import static uno.anahata.asi.nb.tools.java.coderefiner.RelativePosition.AFTER;
import static uno.anahata.asi.nb.tools.java.coderefiner.RelativePosition.BEFORE;
import static uno.anahata.asi.nb.tools.java.coderefiner.RelativePosition.END;
import static uno.anahata.asi.nb.tools.java.coderefiner.RelativePosition.START;

/**
 * The authoritative toolkit for structural Java refinement.
 * <p>
 * This toolkit replaces the legacy path-based CodeRefiner with a
 * batch-oriented, resource-centric approach. It guarantees atomicity and
 * context-integrity through optimistic locking and memory-backed AST
 * simulation.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Advanced structural Java refinement (Batch Mode). Authoritative and robust against Lombok expansions.")
public class BatchCodeRefiner extends AnahataToolkit {

    /**
     * Enabled by default.
     */
    @Override
    public void initialize() {
        getToolkit().setEnabled(true);
    }

    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList(JavaSourceUtils.CANONICAL_FQN_STANDARD
                + "\n"
                + "### BatchCodeRefiner Toolkit Instructions\n"
                + "1. **Context Locked**: You MUST have the resource in your RAG message (context) to propose a refinement.\n"
                + "2. **Batch Intents**: You can combine multiple structural changes (INSERT, UPDATE, DELETE, MOVE) in one call.\n"
                + "3. **Optimistic Locking**: Always use the `lastModified` timestamp from the RAG message. "
                        + "\n\tNote: You can't update the same file twice in the same turn otherwise the first one will change the lastModified and the second one will fail with an optimistic locking exception but you can do as many inserts and updates as you want in a single tool call\n"
                + "4. **Field Initializers**: Put the expression (code after '=') in the `body` field or leave the body empty for fields if you don't want any initialzier expression.\n"
                + "4.1 **Inline Comments**: AST generation natively strips inline comments within method bodies. Use the `Resources` toolkit (`findAndReplaceInTextResource`) for textual edits if you must preserve/inject inline comments.\n"
                + "5. **Javadocs**: Use the specialized Javadocs toolkit for updating documentation.\n"
                + "6. **No imports**: Do not use this tookit to add imports, use CodeRefiner..\n"
                + "7. **No records**: Do not use this tookit to inser or update records, there is a bug in netbeans when adding or updating records using the AST apis, use the Resources toolkit for records.\n"
                + "8. **No training knowledge**: Do not use your training knoweldge, this toolkit is unique to Anahata you have to pay very close attention to the tool definition and the paramters schema.\n"
        );
    }

    /**
     * Refines a Java source file using a robust, flattened batch of structural 
     * AST modifications. This version is recommended for maximum compatibility 
     * across all AI models.
     *
     * @param batch The robust refinement batch.
     * @return The effectively applied changes as a unified diff.
     * @throws Exception if validation or execution fails.
     */
    @AgiTool("The definitive structural Java refiner. "
            + "Applies a batch of member lvel modifications to a java file. "
            + "RelativePosition is mandatory for all INSERT and MOVE. "
            + "When updating a member, you can update both the declaration and the body in the same UPDATE intent or you can just do the body or just the declaration. "
            + "Never include the declaration of a field or a method in the 'body' attribute, the member declaration (signature) can only be in the 'declaration' field only. The 'body' can only contain either whats inside the {} or whatever is to the right of the '='. "
            + "If you update the declaration of a method, you must include the full delcaration with all annotations and all throws clauses. "
            + "Does not support member level javadocs on the 'declaration' (and obvioulsy not in the 'body'). "
            + "Use the javadocs toolkit for adding javadoc in a separate tool call because 1) this tool does not support javadocs and 2) Javadocs.setJavadoc also changes the latModified timestamp on disk"
            + "Does not support java records due to a bug in netbeans. "
            + "It's not a find-and-replace tool, use the Resources toolkit for that. "
            + "You can't use this tool to add imports, just use the fqn of any types not in the imports list with optimize=true to let netbeans import them automatically or use CodeRefiner.addImports to surgically add imports. "
            + "For fields, declaration is what goes to the left of the '=', body is the initializer expression to the right of the '=', leave 'body' empty if you just want to insert a field without initializer expression. You cannot add javadocs to the declaration.")
    public String refine(
            @AgiToolParam("The robust refinement batch.") CodeRefinementBatch batch
    ) throws Exception {
        batch.validate(getAgi());

        Resource resource = getAgi().getResourceManager().get(batch.getResourceUuid());
        NbHandle handle = (NbHandle) resource.getHandle();
        FileObject fo = handle.getFileObject();

        // 1. Calculate the 'Simulated Truth' text using multi-stage memory surgery.
        // This produces a 100% clean result, bypassing the IDE's semantic matcher bug.
        String finalText = batch.calculateResultingContent(getAgi());

        // 2. Singularity Commit: Surgically overwrite the file buffer on disk.
        try (OutputStream os = fo.getOutputStream()) {
            os.write(finalText.getBytes());
        }

        // 3. Sync and Save
        batch.setResultingContent(finalText);
        if (batch.isSave()) {
            JavaSourceUtils.handleSave(fo);
        }

        return batch.getUnifiedDiff(getAgi());
    }

    /**
     * Refines a Java source file using a batch of structural
     * modifications.
     *
     * @param batch The refinement batch containing intents and locking
     * metadata.
     * @return A confirmation message.
     * @throws Exception if validation or execution fails.
     */
    //Commenting this out until models are capable of doing polymorphic / oneOf
    //@AgiTool("Refines a Java source file using a batch of structural AST modifications and returns the effectively applied changes (after user review)")
    /*
    public String refinePolymorphic(
            @AgiToolParam("The refinement batch.") CodeRefinementBatch batch
    ) throws Exception {
        // 1. Authoritative Validation (Recaptures originalContent and checks locks)
        batch.validate(getAgi());

        Resource resource = getAgi().getResourceManager().get(batch.getResourceUuid());
        NbHandle handle = (NbHandle) resource.getHandle();
        FileObject fo = handle.getFileObject();

        JavaSource js = JavaSource.forFileObject(fo);
        ModificationResult result = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);

            String manualOverride = batch.getManualOverride();
            if (manualOverride != null && !manualOverride.isBlank()) {
                // Bypass AST: Apply raw text override from the UI via high-fidelity memory parsing
                log.info("Applying manual text override for {}", fo.getNameExt());
                FileObject tempFo = FileUtil.createMemoryFileSystem().getRoot().createData("Override", "java");
                try (OutputStream os = tempFo.getOutputStream()) {
                    os.write(manualOverride.getBytes());
                }
                JavaSource tempJs = JavaSource.forFileObject(tempFo);
                tempJs.runUserActionTask(info -> {
                    info.toPhase(JavaSource.Phase.PARSED);
                    wc.rewrite(wc.getCompilationUnit(), info.getCompilationUnit());
                }, true);
            } else {
                // Standard structural path
                batch.applyTo(wc);
            }
        });

        result.commit();

        // 3. Capture resulting content snapshot after successful commit
        batch.setResultingContent(resource.asText());

        if (batch.isSave()) {
            JavaSourceUtils.handleSave(fo);
        }

        return batch.getUnifiedDiff(getAgi());
    }
    */
    
    
    /**
     * Internal utility to find a member in the working copy context.
     * 
     * @param wc WorkingCopy
     * @param memberFqn Member FQN
     * @return The leaf Tree node or null.
     */
    public static Tree findMemberInWorkingCopy(org.netbeans.api.java.source.CompilationInfo info, String memberFqn) {
        Tree found = JavaSourceUtils.findTree(info, memberFqn);
        if (found == null) {
            return null;
        }
        TreePath path = TreePath.getPath(info.getCompilationUnit(), found);
        return path != null ? path.getLeaf() : null;
    }

    /**
     * Internal utility to find the index of a specific tree node within a list 
     * of members based on source positions.
     */
    public static int findMemberIndex(org.netbeans.api.java.source.CompilationInfo info, List<? extends Tree> members, Tree target) {
        if (info == null || members == null || target == null) {
            return -1;
        }
        SourcePositions sp = info.getTrees().getSourcePositions();
        CompilationUnitTree cut = info.getCompilationUnit();
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
    private static int findMemberIndex(org.netbeans.api.java.source.CompilationInfo info, List<? extends Tree> members, String memberName) {
        String target = memberName.replaceAll("<[^>]*>", "").replaceAll("\\s+", "");

        for (int i = 0; i < members.size(); i++) {
            Tree m = members.get(i);
            String name = null;
            String signature = null;
            if (m instanceof MethodTree mt) {
                name = mt.getName().toString();
                if (info != null) {
                    TreePath path = TreePath.getPath(info.getCompilationUnit(), m);
                    if (path != null) {
                        Element e = info.getTrees().getElement(path);
                        if (e instanceof ExecutableElement ee) {
                            String params = ee.getParameters().stream().map(p -> {
                                return JavaSourceUtils.getCanonicalFqn(p.asType());
                            }).collect(Collectors.joining(","));
                            signature = (name.equals("<init>") ? "<init>" : name) + "(" + params + ")";
                        }
                    }
                }
                if (signature == null) {
                    String params = mt.getParameters().stream()
                            .map(p -> p.getType().toString().replaceAll("<[^>]*>", "").replaceAll("\\s+", ""))
                            .collect(Collectors.joining(","));
                    signature = (name.equals("<init>") ? "<init>" : name) + "(" + params + ")";
                }
            } else if (m instanceof VariableTree vt) {
                name = vt.getName().toString();
            } else if (m instanceof ClassTree ct) {
                name = ct.getSimpleName().toString();
            } else if (m.getKind() == Tree.Kind.BLOCK) {
                name = ((BlockTree) m).isStatic() ? "<clinit>" : "<init-block>";
            }

            if (name == null) {
                continue;
            }

            String candidateName = name;
            String candidateSig = (signature != null) ? signature.replaceAll("<[^>]*>", "").replaceAll("\\s+", "") : null;

            if (target.equals(candidateName) || target.equals(candidateSig)) {
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
    public static Tree parseMember(WorkingCopy wc, String declaration, String body, ClasspathInfo cpInfo) throws Exception {
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

        // Singularity DNA Propagation: Use the provided CP info to ensure type resolution.
        JavaSource js = cpInfo != null ? JavaSource.create(cpInfo, tempFo) : JavaSource.forFileObject(tempFo);
        
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
    public static int getInsertIndex(org.netbeans.api.java.source.CompilationInfo info, List<? extends Tree> members, RelativePosition position, String anchor) throws AgiToolException {
        int anchorIdx = anchor != null ? findMemberIndex(info, members, getMemberSignature(anchor)) : -1;
        if (anchor != null && anchorIdx == -1) {
            throw new AgiToolException("Anchor member not found: " + anchor);
        }
        return switch (position) {
            case START -> 0;
case END -> Integer.MAX_VALUE;
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
    public static void throwMemberNotFound(org.netbeans.api.java.source.CompilationInfo info, String memberFqn) {
        int paren = memberFqn.indexOf("(");
        String namePart = paren != -1 ? memberFqn.substring(0, paren) : memberFqn;
        int lastSeparator = Math.max(namePart.lastIndexOf("."), namePart.lastIndexOf("$"));
        if (lastSeparator == -1) {
            throw new AgiToolException("Member not found: " + memberFqn + ". Please use the Anahata Canonical Identification FQN standard.");
        }
        String parentFqn = namePart.substring(0, lastSeparator);
        String name = namePart.substring(lastSeparator + 1);
        TypeElement parent = info.getElements().getTypeElement(JavaSourceUtils.normalizeFqn(parentFqn));
        if (parent == null) {
            throw new AgiToolException("Member not found: " + memberFqn + " (Parent class not found: " + parentFqn + "). Ensure nested types use '$' as separator.");
        }
        List<String> candidates = new ArrayList<>();
        for (Element e : parent.getEnclosedElements()) {
            if (e.getSimpleName().contentEquals(name) || (name.equals("<init>") && e.getKind() == ElementKind.CONSTRUCTOR)) {
                candidates.add(JavaSourceUtils.getCanonicalFqn(e));
            }
        }
        StringBuilder sb = new StringBuilder("Member not found: ").append(memberFqn);
        if (!candidates.isEmpty()) {
            sb.append("\nDid you mean one of these canonical identification FQNs?\n");
            candidates.forEach(c-> sb.append("- ").append(c).append("\n"));
        }
        throw new AgiToolException(sb.toString());
    }

}

