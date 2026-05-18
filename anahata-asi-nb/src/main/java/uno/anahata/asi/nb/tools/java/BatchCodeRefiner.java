/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java;

import com.sun.source.tree.BlockTree;
import com.sun.source.tree.ClassTree;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.ExpressionTree;
import com.sun.source.tree.MethodTree;
import com.sun.source.tree.Tree;
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
@AgiToolkit("Advanced structural Java refinement (V4 AST-Guided Batch Mode).")
public class BatchCodeRefiner extends AnahataToolkit {

    @Override
    public void initialize() {
        getToolkit().setEnabled(false);
    }

    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList(JavaSourceUtils.CANONICAL_FQN_STANDARD
                + "\n"
                + "### BatchCodeRefiner Toolkit Instructions (V4 AST-Guided)\n"
                + "1. **Context Locked**: You MUST have the resource in your RAG message (context) to propose a refinement. One resource per tool call.\n"
                + "2. **Batch Intents**: You can combine multiple structural changes (INSERT, UPDATE, DELETE, MOVE) in one call for as long as they all belong to the same java file..\n"
                + "3. **Optimistic Locking**: Always use the `lastModified` timestamp from the RAG message of the resource (java file) you want to modify. \n"
                + "\tNote: You can't update the same java file twice in the same turn using two different BatchCodeRefiner.refine tool calls otherwise the first one will change the lastModified and the second one will fail with an optimistic locking exception but you can do as many inserts and updates as you want in a single tool call\n"
                + "4. **Field Initializers**: Put the expression (code after '=') in the `body` field or leave the body empty for fields if you don't want any initializer expression. For Enum Constants, put the constant name in `declaration` and constructor arguments (if any) in `body`.\n"
                + "5. **Auto-Indentation & Formatting**: Natively supported! V4 computes the exact indentation of the target scope. You do not need to manually pad your `body` with leading spaces. Blank lines and `//` comments within your `body` string are preserved with 100% fidelity.\n"
                + "6. **Javadocs**: Use the structured `javadoc` property (JavadocIntent) to inject Javadocs on the fly. To update ONLY the Javadoc of an existing member, provide the `memberFqn` and the new `javadoc`, leaving `declaration` and `body` null. **WARNING**: If you provide a `javadoc` object during an `UPDATE`, it completely replaces the existing Javadoc. You MUST provide all `@param`, `@return`, and `@throws` fields in the JSON if you want them preserved. If `javadoc` is omitted during an UPDATE, the existing Javadoc is preserved.\n"
                + "7. **Imports**: FQNs provided in `importsToAdd` and `importsToRemove` are safely evaluated and added/removed from the compilation unit.\n"
                + "8. **Records and Modern Java**: Fully supported. Because V4 uses AST-guided text replacement, all modern Java constructs (Records, Switch Expressions, etc.) are safely refactored without breaking the IDE's formatter.\n"
                + "9. **Class-Level Updates**: To update a class declaration (e.g. adding `@Getter` or changing the class Javadoc), set `memberFqn` to the class FQN and `type` to `UPDATE`. Provide the new `declaration` and leave `body` empty. The existing class members will be perfectly preserved!\n"
                + "10. **package-info**: Use the Resources toolkit for creating and editing package-info.java files.\n"
                + "11. **No training knowledge**: Do not use your training knowledge, this toolkit is unique to Anahata you have to pay very close attention to the tool definition and the parameters schema.\n"

        );
    }

    @AgiTool("The definitive structural Java refiner. Applies a batch of member-level modifications to ONE java file. RelativePosition is mandatory for all INSERT and MOVE. When updating a member, you can update both the declaration and the body in the same UPDATE intent or you can just do the body or just the declaration. Never include the declaration of a field or a method in the 'body' attribute, the member declaration (signature) can only be in the 'declaration' field only. The 'body' can only contain either whats inside the {} or whatever is to the right of the '='. If you update the declaration of a method, you must include the full declaration with all annotations and all throws clauses. Provides a fully integrated `javadoc` object property so you can document members synchronously with code changes! Inline comments inside the `body` string are natively preserved! It is not a find-and-replace tool, use the Resources toolkit for that. You CAN use this tool to add or remove imports via the importsToAdd and importsToRemove arrays. For fields, declaration is what goes to the left of the '=', body is the initializer expression to the right of the '=', leave 'body' empty if you just want to insert a field without initializer expression. For Enum Constants, put the constant name in `declaration` and constructor arguments in `body`. Do not put javadoc strings inside the `declaration` field, use the structured `javadoc` parameter instead.")
    public String refine(
            @AgiToolParam("The robust refinement batch.") CodeRefinementBatch batch
    ) throws Exception {
        batch.validate(getAgi());

        Resource resource = getAgi().getResourceManager().get(batch.getResourceUuid());
        NbHandle handle = (NbHandle) resource.getHandle();
        FileObject fo = handle.getFileObject();

        String finalText = batch.calculateResultingContent(getAgi());

        try (OutputStream os = fo.getOutputStream()) {
            os.write(finalText.getBytes());
        }

        batch.setResultingContent(finalText);
        if (batch.isSave()) {
            JavaSourceUtils.handleSave(fo);
        }

        return batch.getUnifiedDiff(getAgi());
    }

    /**
     * Locates a member tree within a working copy by its canonical FQN.
     */
    public static Tree findMemberInWorkingCopy(org.netbeans.api.java.source.CompilationInfo info, String memberFqn) {
        return JavaSourceUtils.findTree(info, memberFqn);
    }

    /**
     * Finds the index of a member within a list of trees by its signature.
     */
    public static int findMemberIndex(org.netbeans.api.java.source.CompilationInfo info, List<? extends Tree> members, String memberName) {
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
                                javax.lang.model.type.TypeMirror tm = p.asType();
                                return tm != null ? JavaSourceUtils.getCanonicalFqn(tm) : "Unknown";
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
     * Parses a raw Java string into a detached AST Tree node, capturing inline comments.
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
                    org.netbeans.api.java.source.GeneratorUtilities gu = org.netbeans.api.java.source.GeneratorUtilities.get(wc);
                    com.sun.source.tree.Tree importedTree = gu.importComments(t, innerWc.getCompilationUnit());
                    com.sun.source.tree.Tree newTree = wc.getTreeMaker().asNew(importedTree);
                    gu.copyComments(importedTree, newTree, true);
                    result[0] = newTree;
                }
            }
        }, true);
        return result[0];
    }

    /**
     * Calculates the insertion index for a new member based on a relative position and an anchor.
     */
    public static int getInsertIndex(org.netbeans.api.java.source.CompilationInfo wc, List<? extends Tree> members, RelativePosition position, String anchor) throws AgiToolException {
        if ((position == RelativePosition.BEFORE || position == RelativePosition.AFTER) && (anchor == null || anchor.isBlank())) {
            throw new AgiToolException("anchorMemberName is mandatory for relative position " + position);
        }
        int anchorIdx = -1;
        if (anchor != null && !anchor.isBlank()) {
            anchorIdx = findMemberIndex(wc, members, getMemberSignature(anchor));
            if (anchorIdx == -1) {
                throw new AgiToolException("Anchor member not found: " + anchor);
            }
        }
        return switch (position) {
            case START -> 0;
            case END -> members.size();
            case BEFORE -> anchorIdx;
            case AFTER -> anchorIdx + 1;
        };
    }
    
    /**
     * Extracts the raw signature from a canonical FQN to be used for matching.
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
     * Throws a highly descriptive AgiToolException with available candidates when a member is not found.
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

    /**
     * Rebuilds a ClassTree node with a new list of members.
     */
    public static ClassTree rebuildClassTree(TreeMaker make, ClassTree ct, List<Tree> members) {
        return switch (ct.getKind()) {
            case INTERFACE -> make.Interface(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(), ct.getImplementsClause(), ct.getPermitsClause(), members);
            case ENUM -> make.Enum(ct.getModifiers(), ct.getSimpleName(), (List<ExpressionTree>) (List<?>) ct.getImplementsClause(), members);
            case ANNOTATION_TYPE -> make.AnnotationType(ct.getModifiers(), ct.getSimpleName(), members);
            case RECORD -> make.Class(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(), null, (List<ExpressionTree>) (List<?>) ct.getImplementsClause(), (List<ExpressionTree>) (List<?>) ct.getPermitsClause(), members);
            default -> make.Class(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(), ct.getExtendsClause(), (List<ExpressionTree>) (List<?>) ct.getImplementsClause(), (List<ExpressionTree>) (List<?>) ct.getPermitsClause(), members);
        };
    }

}