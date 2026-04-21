/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java;

import com.sun.source.tree.*;
import com.sun.source.util.TreePathScanner;
import java.io.IOException;
import java.io.OutputStream;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import javax.lang.model.element.*;
import javax.lang.model.type.TypeKind;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.source.*;
import org.netbeans.modules.editor.indent.api.Reformat;
import org.netbeans.modules.java.hints.spiimpl.hints.HintsInvoker;
import org.netbeans.modules.java.hints.spiimpl.options.HintsSettings;
import org.netbeans.spi.editor.hints.ErrorDescription;
import org.netbeans.spi.editor.hints.Fix;
import org.openide.cookies.EditorCookie;
import org.openide.cookies.SaveCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import uno.anahata.asi.agi.tool.*;
import uno.anahata.asi.nb.tools.java.JavaSourceUtils.RelativePosition;
import org.netbeans.api.java.source.ClassIndex;
import org.netbeans.api.java.source.ElementHandle;
import javax.lang.model.element.TypeElement;
import com.sun.source.util.TreePath;
import javax.swing.text.Document;
import org.netbeans.api.java.source.support.ReferencesCount;
import org.netbeans.modules.editor.java.Utilities;

/**
 * V2.7 of the structural Java code refinement toolkit. High-fidelity structural
 * manipulation using full declarations and AST-based rewriting. Fixes the AST
 * duplication poltergeist by avoiding overlapping rewrites and stripping
 * comment offsets during migration.
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Structural Java code refinement (V2). Uses full declarations for high-fidelity AST-based updates.")
public class CodeRefiner2 extends AnahataToolkit {

    @Override
    public void initialize() {
        getToolkit().setEnabled(false);
    }

    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList("CodeRefiner2: The authority for structural Java changes."
                + "\n- 'declaration' must be the full signature (e.g., '@Override public void foo(int a) throws IOException'). It MUST include any annotations (e.g., @Override, @Deprecated) you want to preserve or add."
                + "\n- If 'declaration' is omitted in updateMember, the existing signature is preserved (Surgical Mode)."
                + "\n- 'body' is the content for the member: for methods/blocks, it's the logic inside the braces; for fields, it's the initializer expression. If omitted during update, the original content is preserved."
                + "\n- 'javadoc' content MUST NOT include the /** and */ markers. The tool adds them automatically. If omitted, it remains unchaged."
                + "\n- Identification Standard: 'memberFqn' must be ABSOLUTE (e.g. 'com.foo.MyClass.myField', 'com.foo.MyClass.myMethod(java.lang.String,int)', 'com.foo.MyClass$InnerClass.member', 'com.foo.MyClass.<clinit>#1()' or 'com.foo.MyClass.<init-block>#2()')."
                + "\n- METHODS/CONSTRUCTORS: Parentheses '()' are MANDATORY, even if there are no parameters (e.g. 'myMethod()')."
                + "\n- OVERLOADS: You MUST provide the canonical FQN of all parameters to disambiguate (e.g. 'java.lang.String,com.foo.MyClass$Inner')."
                + "\n- ARRAYS: Use '[]' for array parameters (e.g. 'java.lang.String[]', 'int[]')."
                + "\n- INITIALIZERS: Use <clinit>#n() for static blocks and <init-block>#n() for instance blocks, where n is the 1-based occurrence index in the class."
                + "\n- RULES FOR METHOD IDENTIFICATION: Parameter types must be CANONICAL: Fully Qualified, NO Generics, NO Annotations. Example: 'com.foo.Bar.process(java.util.List,int)'."
                + "\n- RelativePosition is MANDATORY for insertion/move. anchorMemberName is MANDATORY if position is BEFORE/AFTER."
                + "\n- ANCHORS: anchorMemberName must be RELATIVE to the class (e.g. 'myField', 'myMethod()', '<clinit>#1()' or '<init-block>#1()')."
                + "\n- records: insertMember and updateMember do not work with records due to a known nb bugs. Use the resources toolkit for records.");
    }

    @AgiTool("Inserts a new member into a class. Does not work with Records.")
    public String insertMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the target class. Use '$' for nested types (e.g. 'com.foo.Outer$Inner'). If empty, targets the file level.") String classFqn,
            @AgiToolParam(value = "The full member declaration/signature (e.g. '@Deprecated private String name;' or '@Override public void foo(String s) throws IOException'). It MUST include all annotations you wish to apply. Do NOT include the body code here.", rendererId = "java") String declaration,
            @AgiToolParam(value = "The WHOLE body code (logic inside the braces). Do NOT include the signature or the outer braces. This must be the entire content for the new member.", rendererId = "java", required = false) String body,
            @AgiToolParam(value = "The Javadoc content (WITHOUT /** and */ markers). The tool adds the markers automatically.", required = false) String javadoc,
            @AgiToolParam("Position relative to anchor.") RelativePosition position,
            @AgiToolParam(value = "Anchor member name relative to class (e.g. 'myField', 'myMethod()', 'foo(java.lang.String[])', '<clinit>#1()').", required = false) String anchorMemberName,
            @AgiToolParam(value = "Whether to optimize imports after insertion. Defaults to true.", required = false) Boolean optimize,
            @AgiToolParam("Whether to save the file.") boolean save) throws Exception {

        validatePosition(position, anchorMemberName);
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);
        final Set<String> diagnostics = new LinkedHashSet<>();
        ModificationResult res = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            Tree parentTree;
            List<Tree> members;
            if (classFqn == null || classFqn.isBlank()) {
                parentTree = wc.getCompilationUnit();
                members = new ArrayList<>(((CompilationUnitTree) parentTree).getTypeDecls());
            } else {
                Element resolved = JavaSourceUtils.findElement(wc, classFqn);
                if (!(resolved instanceof TypeElement te)) {
                    throw new AgiToolException("Class not found or invalid: " + classFqn);
                }
                parentTree = (ClassTree) wc.getTrees().getTree(te);
                members = new ArrayList<>(((ClassTree) parentTree).getMembers());
            }

            Tree newMember = parseMember(wc, declaration, body);
            applyJavadocStructural(wc, newMember, javadoc, null);

            if (optimize != null && optimize) {
                newMember = GeneratorUtilities.get(wc).importFQNs(newMember);
            }

            int insertIdx = getInsertIndex(wc, members, position, anchorMemberName);
            members.add(insertIdx, newMember);

            if (parentTree instanceof ClassTree ct) {
                wc.rewrite(ct, rebuildClassTree(make, ct, members));
            } else if (parentTree instanceof CompilationUnitTree cut) {
                CompilationUnitTree updated = make.CompilationUnit(cut.getPackage(), cut.getImports(), (List<Tree>) (List<?>) members, cut.getSourceFile());
                wc.rewrite(cut, updated);
            }
        });
        res.commit();

        if (optimize == null || optimize) {
            optimizeImportsInternal(js, true);
        }

        if (save) {
            handleSave(fo);
        }
        return "Inserted member into " + (classFqn == null || classFqn.isBlank() ? "file level" : classFqn);
    }

    @AgiTool("Updates an existing member structurally (Signature, Body, or Javadoc). Does not work with Records (due to a known nb bug, use the Resources toolkit for updating or inserting records)."
            + " Do not provide the body or the signature if you just want to update the javadoc. "
            + " Do not provide the declaration or the javadoc if you just want to update the body. "
            + " Do not provide the body or the javadoc if you just want to update the declaration. "
            + "")
    public String updateMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the member. Use 'package.Class.method(param1,param2)' or 'package.Class.method()' for no-arg methods. You MUST provide all parameter FQNs (e.g. 'java.lang.String,com.foo.MyClass$Inner'). Use 'package.Class$Inner' for types and 'package.Class.<clinit>#1()' for blocks.") String memberFqn,
            @AgiToolParam(value = "The new member declaration (signature). Do not provide it if it doesn't need to change. NEVER include the body here. If provided, it MUST include all annotations you wish to preserve (e.g., @Override, @Deprecated).", required = false, rendererId = "java") String declaration,
            @AgiToolParam(value = "The WHOLE body code (logic inside the braces for methods/blocks; for fields, it's the initializer expression). Do not provide it if it doesn't need to change. Do NOT include the signature or outer braces. To surgically change a fragment, use the Resources toolkit instead.", rendererId = "java", required = false) String body,
            @AgiToolParam(value = "Optional new Javadoc (WITHOUT /** and */ markers). Do not provide it if it doesn't need to change. The tool adds markers automatically.", required = false) String javadoc,
            @AgiToolParam(value = "Whether to optimize imports after update. Defaults to true.", required = false) Boolean optimize,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);
        final Set<String> diagnostics = new LinkedHashSet<>();
        ModificationResult res = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            Tree oldTree = JavaSourceUtils.findTree(wc, memberFqn);
            if (oldTree == null) {
                throwMemberNotFound(wc, memberFqn);
            }

            applyJavadocStructural(wc, oldTree, javadoc, null);

            if (declaration != null || body != null) {
                Tree newTree;
                if (declaration == null) {
                    newTree = cloneTree(make, oldTree);
                    if (body != null) {
                        if (newTree instanceof MethodTree mt) {
                            newTree = make.Method(mt.getModifiers(), mt.getName(), mt.getReturnType(), mt.getTypeParameters(), mt.getParameters(), mt.getThrows(), parseBody(wc, body), (AnnotationTree) mt.getDefaultValue());
                        } else if (newTree instanceof VariableTree vt) {
                            ExpressionTree finalInit = wc.getTreeUtilities().parseExpression(body, null);
                            newTree = make.Variable(vt.getModifiers(), vt.getName(), vt.getType(), finalInit);
                        } else if (newTree instanceof BlockTree bt) {
                            BlockTree parsed = parseBody(wc, body);
                            newTree = make.Block(parsed.getStatements(), bt.isStatic());
                        }
                    }
                } else {
                    newTree = parseMember(wc, declaration, body);
                }
                rewriteMember(wc, oldTree, newTree);
            }
});
        res.commit();

        if (optimize == null || optimize) {
            optimizeImportsInternal(js, true);
        }

        if (save) {
            handleSave(fo);
        }
        return "Updated member " + memberFqn;
    }

    @AgiTool("Removes a member structurally.")
    public String deleteMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the member (e.g. 'package.Class.method(java.lang.String,int)', 'package.Class$Inner', 'package.Class.<clinit>#1()'). Parentheses '()' are mandatory for executables.") String memberFqn,
            @AgiToolParam(value = "Whether to optimize imports after deleting the member. Defaults to true.", required = false) Boolean optimize,
            @AgiToolParam("Whether to save the file.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);
        final Set<String> diagnostics = new LinkedHashSet<>();
        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            Tree memberTree = JavaSourceUtils.findTree(wc, memberFqn);
            if (memberTree == null) {
                throwMemberNotFound(wc, memberFqn);
            }

            TreePath path = TreePath.getPath(wc.getCompilationUnit(), memberTree);
            Tree parent = path.getParentPath().getLeaf();

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

        }).commit();
        
        if (optimize == null || optimize) {
            optimizeImportsInternal(js, true);
        }

        if (save) {
            handleSave(fo);
        }
        return "Removed member '" + memberFqn + "' structurally.";
    }

    @AgiTool("Moves a member to a new position within the same class.")
    public String moveMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the member to move (e.g. 'package.Class.method(java.lang.String,int)', 'package.Class$Inner', 'package.Class.<clinit>#1()'). Parentheses '()' are mandatory for executables.") String memberFqn,
            @AgiToolParam("Position relative to anchor. (START, END, BEFORE, AFTER)") RelativePosition position,
            @AgiToolParam(value = "Anchor member name relative to class (e.g. 'myField', 'myMethod(int)', '<clinit>#1()').", required = false) String anchorMemberName,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        validatePosition(position, anchorMemberName);
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            Tree memberTree = JavaSourceUtils.findTree(wc, memberFqn);
            if (memberTree == null) {
                throwMemberNotFound(wc, memberFqn);
            }

            Element element = wc.getTrees().getElement(TreePath.getPath(wc.getCompilationUnit(), memberTree));
            Element parentElement = element != null ? element.getEnclosingElement() : null;
            if (parentElement == null) {
                // Try to find parent via TreePath
                TreePath path = TreePath.getPath(wc.getCompilationUnit(), memberTree);
                if (path != null && path.getParentPath() != null) {
                    parentElement = wc.getTrees().getElement(path.getParentPath());
                }
            }
            if (!(parentElement instanceof TypeElement te)) {
                throw new AgiToolException("Only members of a class can be moved.");
            }

            ClassTree parentTree = (ClassTree) wc.getTrees().getTree(te);
            List<Tree> members = new ArrayList<>(parentTree.getMembers());
            members.remove(memberTree);

            int insertIdx = getInsertIndex(wc, members, position, anchorMemberName);
            members.add(insertIdx, memberTree);

            ClassTree updatedParent = rebuildClassTree(make, parentTree, members);
            wc.rewrite(parentTree, updatedParent);
        }).commit();

        if (save) {
            handleSave(fo);
        }
        return "Moved member '" + memberFqn + "' to " + position;
    }

    /**
     * Adds one or more imports to a file structurally.
     */
    @AgiTool("Adds one or more imports to a file structurally.")
    public String addImports(@AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath, @AgiToolParam("List of FQNs to import.") List<String> imports, @AgiToolParam("Whether to save.") boolean save) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);
        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            CompilationUnitTree cut = wc.getCompilationUnit();
            List<ImportTree> currentImports = new ArrayList<>(cut.getImports());
            for (String imp : imports) {
                String cleanImp = imp.trim();
                boolean isStatic = cleanImp.startsWith("static ");
                if (isStatic) {
                    cleanImp = cleanImp.substring(7).trim();
                }
                final String finalImp = cleanImp;
                boolean exists = currentImports.stream().anyMatch(i -> i.isStatic() == isStatic && i.getQualifiedIdentifier().toString().equals(finalImp));
                if (!exists) {
                    currentImports.add(make.Import(make.Identifier(finalImp), isStatic));
                }
            }
            CompilationUnitTree updated = make.CompilationUnit(cut.getPackage(), currentImports, cut.getTypeDecls(), cut.getSourceFile());
            wc.rewrite(cut, updated);
        }).commit();
        if (save) {
            handleSave(fo);
        }
        return "Added " + imports.size() + " import(s) to " + fo.getNameExt();
    }

    @AgiTool("Reformats a file using IDE rules. Only works if the file is open in the editor")
    public String reformat(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        DataObject doid = DataObject.find(fo);
        EditorCookie ec = doid.getLookup().lookup(EditorCookie.class);
        if (ec == null || ec.getOpenedPanes() == null || ec.getOpenedPanes().length == 0) {
            throw new AgiToolException("File is not open in the editor: " + filePath);
        }

        JavaSource js = JavaSource.forFileObject(fo);
        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            Document doc = wc.getDocument();
            if (doc != null) {
                Reformat reformat = Reformat.get(doc);
                reformat.lock();
                try {
                    reformat.reformat(0, doc.getLength());
                } finally {
                    reformat.unlock();
                }
            }
        }).commit();

        if (save) {
            handleSave(fo);
        }
        return "Reformated: " + fo.getNameExt();
    }

    @AgiTool("Optimizes imports (converts FQNs to simple names, removes unused).")
    public String optimizeImports(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath, @AgiToolParam(value = "Whether to remove unused imports.", required = false) Boolean removeUnused, @AgiToolParam("Whether to save.") boolean save) throws Exception {

        final boolean doRemove = removeUnused == null || removeUnused;
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        optimizeImportsInternal(js, doRemove);

        if (save) {
            handleSave(fo);
        }
        return "Optimized imports for: " + fo.getNameExt() + ". Check logs for details.";
    }

    @AgiTool("Sets or updates Javadoc for a class, field, constructor, method or any member in general. Note: For package-info.java files, use the Resources toolkit instead.")
    public String setJavadoc(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the member.") String memberFqn,
            @AgiToolParam(value = "The main Javadoc description.", required = false) String javadoc,
            @AgiToolParam(value = "Optional list of block tags (e.g. ['author Pablo', 'since 2026']).", required = false) List<String> tags,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();

            Tree oldTree = JavaSourceUtils.findTree(wc, memberFqn);
            if (oldTree == null) {
                throwMemberNotFound(wc, memberFqn);
            }

            // For setJavadoc tool, we perform a structural Javadoc rewrite directly
            applyJavadocStructural(wc, oldTree, javadoc, tags);
        }).commit();

        if (save) {
            handleSave(fo);
        }
        return "Set Javadoc for " + memberFqn;
    }

    /**
     * Applies Javadoc content to a target tree using the high-level structural DocTree API.
     * This ensures perfect asterisk alignment and IDE-compliant formatting.
     */
    private void applyJavadocStructural(WorkingCopy wc, Tree tree, String description, List<String> tagInputs) {
        TreeMaker make = wc.getTreeMaker();
        com.sun.source.util.DocTrees docTrees = wc.getDocTrees();
        CompilationUnitTree cut = wc.getCompilationUnit();
        com.sun.source.util.TreePath path = wc.getTrees().getPath(cut, tree);
        
        com.sun.source.doctree.DocCommentTree oldDoc = docTrees.getDocCommentTree(path);

        List<com.sun.source.doctree.DocTree> body = new ArrayList<>();
        if (description != null && !description.isBlank()) {
            body.add(make.Text(description));
        }

        List<com.sun.source.doctree.DocTree> tags = new ArrayList<>();
        if (tagInputs != null) {
            for (String input : tagInputs) {
                int firstSpace = input.indexOf(' ');
                String tagName = (firstSpace != -1) ? input.substring(0, firstSpace) : input;
                String content = (firstSpace != -1) ? input.substring(firstSpace + 1) : "";

                switch (tagName.toLowerCase()) {
                    case "author" -> tags.add(make.Author(Collections.singletonList(make.Text(content))));
                    case "since" -> tags.add(make.Since(Collections.singletonList(make.Text(content))));
                    case "version" -> tags.add(make.Version(Collections.singletonList(make.Text(content))));
                    case "param" -> {
                        int nameSpace = content.indexOf(' ');
                        String pName = (nameSpace != -1) ? content.substring(0, nameSpace) : content;
                        String pDesc = (nameSpace != -1) ? content.substring(nameSpace + 1) : "";
                        boolean isType = pName.startsWith("<") && pName.endsWith(">");
                        tags.add(make.Param(isType, make.DocIdentifier(pName), Collections.singletonList(make.Text(pDesc))));
                    }
                    case "return" -> tags.add(make.DocReturn(Collections.singletonList(make.Text(content))));
                    case "throws", "exception" -> tags.add(make.Throws(make.Reference(null, content, null), Collections.singletonList(make.Text(""))));
                    default -> tags.add(make.UnknownBlockTag(tagName, Collections.singletonList(make.Text(content))));
                }
            }
        }

        com.sun.source.doctree.DocCommentTree newDoc = make.DocComment(body, tags);
        wc.rewrite(tree, oldDoc, newDoc);
    }

    /**
     * Validates the consistency of the relative position and anchor member name
     * for insertion and move operations.
     *
     * @param position the relative position
     * @param anchor the anchor member name
     * @throws AgiToolException if the position and anchor are inconsistent
     */
    private void validatePosition(RelativePosition position, String anchor) throws AgiToolException {
        if (position == null) {
            throw new AgiToolException("RelativePosition is mandatory.");
        }
        if ((position == RelativePosition.BEFORE || position == RelativePosition.AFTER) && (anchor == null || anchor.isBlank())) {
            throw new AgiToolException("anchorMemberName is mandatory for position " + position);
        }
    }

    /**
     * Rebuilds a ClassTree with a new list of members, preserving the original
     * modifiers, name, and clauses (extends, implements, permits). Supports
     * Class, Interface, Enum, and Annotation types.
     *
     * @param make the tree maker
     * @param ct the original class tree
     * @param members the new list of member trees
     * @return the newly constructed ClassTree
     */
    private ClassTree rebuildClassTree(TreeMaker make, ClassTree ct, List<Tree> members) {
        return switch (ct.getKind()) {
            case INTERFACE ->
                make.Interface(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(), (List<ExpressionTree>) (List<?>) ct.getImplementsClause(), (List<ExpressionTree>) (List<?>) ct.getPermitsClause(), members);
            case ENUM ->
                make.Enum(ct.getModifiers(), ct.getSimpleName(), (List<ExpressionTree>) (List<?>) ct.getImplementsClause(), members);
            case ANNOTATION_TYPE ->
                make.AnnotationType(ct.getModifiers(), ct.getSimpleName(), members);
            case RECORD ->
                make.Class(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(), null, (List<ExpressionTree>) (List<?>) ct.getImplementsClause(), (List<ExpressionTree>) (List<?>) ct.getPermitsClause(), members);
            default ->
                make.Class(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(), ct.getExtendsClause(), (List<ExpressionTree>) (List<?>) ct.getImplementsClause(), (List<ExpressionTree>) (List<?>) ct.getPermitsClause(), members);
        };
    }

    /**
     * Parses a string-based member declaration and optional body into a
     * {@link Tree}. This is achieved by creating a temporary file in a memory
     * filesystem and running a PARSED phase task.
     *
     * @param wc the working copy
     * @param declaration the member declaration string
     * @param body the member body string (optional)
     * @return the parsed tree
     * @throws Exception if parsing fails
     */
    private Tree parseMember(WorkingCopy wc, String declaration, String body) throws Exception {
        String decl = declaration.trim();
        boolean isStandaloneType = decl.startsWith("record ") || decl.contains(" record ") || decl.startsWith("class ") || decl.contains(" class ") || decl.startsWith("interface ") || decl.contains(" interface ") || decl.startsWith("enum ") || decl.contains(" enum ");
        if (!decl.endsWith(";") && !decl.endsWith("}")) {
            if (decl.contains("(")) {
                String b = (body == null) ? "{}" : (body.trim().startsWith("{") ? body : "{" + body + "}");
                decl += " " + b;
            } else {
                decl += ";";
            }
        }
        final String finalDecl = decl;
        String dummyClassName = isStandaloneType ? "DummyType" : "__Dummy";
        if (decl.contains("(") && !decl.contains(" ")) {
            String firstPart = decl.substring(0, decl.indexOf('(')).trim();
            if (firstPart.matches("[a-zA-Z0-9_]+")) {
                dummyClassName = firstPart;
            }
        }
        FileObject tempFo = FileUtil.createMemoryFileSystem().getRoot().createData(dummyClassName, "java");
        String dummyCode = isStandaloneType ? finalDecl : "class " + dummyClassName + " { " + finalDecl + " }";
        try (OutputStream os = tempFo.getOutputStream()) {
            os.write(dummyCode.getBytes());
        }
        JavaSource js = JavaSource.forFileObject(tempFo);
        final Tree[] result = new Tree[1];
        js.runUserActionTask(cc -> {
            cc.toPhase(JavaSource.Phase.PARSED);
            CompilationUnitTree cut = cc.getCompilationUnit();
            if (!cut.getTypeDecls().isEmpty()) {
                if (isStandaloneType) {
                    result[0] = cut.getTypeDecls().get(0);
                } else {
                    ClassTree ct = (ClassTree) cut.getTypeDecls().get(0);
                    for (Tree member : ct.getMembers()) {
                        if (member instanceof MethodTree mt && mt.getName().contentEquals("<init>") && !finalDecl.contains("<init>")) {
                            if (ct.getMembers().size() > 1) {
                                continue;
                            }
                        }
                        result[0] = member;
                        break;
                    }
                }
            }
        }, true);
        return result[0];
    }


    /**
     * Performs a structural rewrite of a member by rewriting its parent (Class
     * or CU). This linearizes the rewrite instructions and prevents
     * 'Overlapping Rewrite' conflicts when GeneratorUtilities.importFQNs has
     * already registered a global rewrite.
     *
     * @param wc the working copy
     * @param oldMember the member to replace
     * @param newMember the new member version
     */
    private void rewriteMember(WorkingCopy wc, Tree oldMember, Tree newMember) {
        TreePath path = TreePath.getPath(wc.getCompilationUnit(), oldMember);
        if (path == null) {
            log("rewriteMember could not determine path ");
            wc.rewrite(oldMember, newMember); // Fallback
            return;
        }
        log("rewriteMember found TreePath " + path);
        Tree parent = path.getParentPath().getLeaf();
        TreeMaker make = wc.getTreeMaker();

        if (parent instanceof ClassTree ct) {
            List<Tree> members = new ArrayList<>(ct.getMembers());
            int idx = members.indexOf(oldMember);
            if (idx == -1) {
                // Identity mismatch, use surgical rewrite as fallback
                wc.rewrite(oldMember, newMember);
            } else {
                members.set(idx, newMember);
                wc.rewrite(ct, rebuildClassTree(make, ct, members));
            }
        } else if (parent instanceof CompilationUnitTree cut) {
            List<Tree> types = new ArrayList<>(cut.getTypeDecls());
            int idx = types.indexOf(oldMember);
            if (idx == -1) {
                wc.rewrite(oldMember, newMember);
            } else {
                types.set(idx, newMember);
                CompilationUnitTree updated = make.CompilationUnit(cut.getPackage(), cut.getImports(), types, cut.getSourceFile());
                wc.rewrite(cut, updated);
            }
        } else {
            wc.rewrite(oldMember, newMember);
        }
    }

    /**
     * Clones a tree node for structural rewrite, ensuring a new memory identity
     * while preserving the original structure. This is critical for triggering
     * the Rewriter to process comments and metadata attached to the node.
     *
     * @param make the tree maker
     * @param tree the original tree to clone
     * @return a new tree instance with the same structure
     */
    private Tree cloneTree(TreeMaker make, Tree tree) {
        if (tree instanceof ClassTree ct) {
            return rebuildClassTree(make, ct, new ArrayList<>(ct.getMembers()));
        } else if (tree instanceof MethodTree mt) {
            return make.Method(mt.getModifiers(), mt.getName(), mt.getReturnType(), mt.getTypeParameters(), mt.getParameters(), mt.getThrows(), mt.getBody(), (AnnotationTree) mt.getDefaultValue());
        } else if (tree instanceof VariableTree vt) {
            return make.Variable(vt.getModifiers(), vt.getName(), vt.getType(), vt.getInitializer());
        } else if (tree instanceof BlockTree bt) {
            return make.Block(bt.getStatements(), bt.isStatic());
        }
        return tree;
    }

    private void removeUnusedImportsInternal(WorkingCopy copy) {
        try {
            HintsSettings settings = HintsSettings.getSettingsFor(copy.getFileObject());
            HintsInvoker invoker = new HintsInvoker(settings, new AtomicBoolean());
            List<ErrorDescription> hints = invoker.computeHints(copy);

            if (hints != null) {
                for (ErrorDescription ed : hints) {
                    if ("Imports_UNUSED".equals(ed.getId())) {
                        List<Fix> fixes = ed.getFixes().getFixes();
                        if (fixes != null && !fixes.isEmpty()) {
                            fixes.get(0).implement();
                        }
                    }
                }
            }
        } catch (Exception e) {
            error("Failed to remove unused imports using HintsInvoker");
            log.error("Failed to remove unused imports using HintsInvoker", e);
        }
    }

    /**
     * Internal implementation of the import optimization logic. Performs a
     * two-phase process: converting FQNs to simple names via GeneratorUtilities
     * and scanning for unresolved identifiers to provide ranked candidates
     * based on NetBeans importance levels.
     */
    private void optimizeImportsInternal(JavaSource js, boolean removeUnused) throws Exception {
        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            ReferencesCount rc = ReferencesCount.get(wc.getClasspathInfo());
            CompilationUnitTree oldCut = wc.getCompilationUnit();
            CompilationUnitTree newCut = GeneratorUtilities.get(wc).importFQNs(oldCut);
            wc.rewrite(oldCut, newCut);
            if (removeUnused) {
                removeUnusedImportsInternal(wc);
            }
            new TreePathScanner<Void, WorkingCopy>() {
                @Override
                public Void visitIdentifier(IdentifierTree node, WorkingCopy wc) {
                    TreePath path = getCurrentPath();
                    if (path != null) {
                        Element e = wc.getTrees().getElement(path);
                        if (e == null || (e.asType() != null && e.asType().getKind() == TypeKind.ERROR)) {
                            String name = node.getName().toString();
                            if (Character.isUpperCase(name.charAt(0))) {
                                CodeRefiner2.this.log("Unresolved type: " + name);
                                try {
                                    ClassIndex index = wc.getClasspathInfo().getClassIndex();
                                    Set<ClassIndex.SearchScope> scopes = EnumSet.allOf(ClassIndex.SearchScope.class);
                                    Set<ElementHandle<TypeElement>> handles = index.getDeclaredTypes(name, ClassIndex.NameKind.SIMPLE_NAME, scopes);
                                    if (!handles.isEmpty()) {
                                        class Candidate {

                                            final String fqn;
                                            final int score;

                                            Candidate(String fqn, int score) {
                                                this.fqn = fqn;
                                                this.score = score;
                                            }
                                        }
                                        List<Candidate> candidates = new ArrayList<>();
                                        for (ElementHandle<TypeElement> handle : handles) {
                                            TypeElement te = handle.resolve(wc);
                                            int score = te != null ? Utilities.getImportanceLevel(wc, rc, te) : Utilities.getImportanceLevel(handle.getQualifiedName());
                                            candidates.add(new Candidate(handle.getQualifiedName(), score));
                                        }
                                        candidates.sort(Comparator.comparingInt(c -> c.score));
                                        CodeRefiner2.this.log("Found " + candidates.size() + " candidates for " + name + " (NetBeans Importance sort):");
                                        candidates.forEach(c -> CodeRefiner2.this.log(" - " + c.fqn));
                                    }
                                } catch (Exception ex) {
                                    log.error("OptimizeImports: Index search failed for " + name, ex);
                                }
                            }
                        }
                    }
                    return super.visitIdentifier(node, wc);
                }
            }.scan(new TreePath(wc.getCompilationUnit()), wc);
        }).commit();
    }

    /**
     * Ensures that all changes are persisted to disk by invoking both
     * EditorCookie (for open files) and SaveCookie (for project data), followed
     * by a file system refresh.
     *
     * @param fo the file object to save
     * @throws IOException if any persistence error occurs
     */
    private void handleSave(FileObject fo) throws IOException {
        DataObject doid = DataObject.find(fo);
        EditorCookie ec = doid.getLookup().lookup(EditorCookie.class);
        if (ec != null && ec.getOpenedPanes() != null) {
            ec.saveDocument();
        }
        SaveCookie sc = doid.getLookup().lookup(SaveCookie.class);
        if (sc != null) {
            sc.save();
        }
        fo.refresh();
    }

    /**
     * Throws an AgiToolException when a requested member cannot be found,
     * providing a list of canonical candidate FQNs to assist the model.
     *
     * @param wc the working copy
     * @param memberFqn the FQN that was not found
     * @throws AgiToolException always
     */
    private void throwMemberNotFound(WorkingCopy wc, String memberFqn) throws AgiToolException {
        int paren = memberFqn.indexOf("(");
        String namePart = paren != -1 ? memberFqn.substring(0, paren) : memberFqn;
        int lastSeparator = Math.max(namePart.lastIndexOf("."), namePart.lastIndexOf("$"));
        if (lastSeparator == -1) {
            throw new AgiToolException("Member not found: " + memberFqn);
        }

        String parentFqn = namePart.substring(0, lastSeparator);
        String name = namePart.substring(lastSeparator + 1);

        TypeElement parent = wc.getElements().getTypeElement(parentFqn);
        if (parent == null) {
            throw new AgiToolException("Member not found: " + memberFqn + " (Parent class not found: " + parentFqn + ")");
        }

        List<String> candidates = new ArrayList<>();
        for (Element e : parent.getEnclosedElements()) {
            if (e.getSimpleName().contentEquals(name)) {
                if (e instanceof ExecutableElement ee) {
                    String params = ee.getParameters().stream()
                            .map(p -> {
                                String type = p.asType().toString();
                                return type.contains("<") ? type.substring(0, type.indexOf("<")) : type;
                            })
                            .collect(Collectors.joining(","));
                    candidates.add(parentFqn + "." + name + "(" + params + ")");
                } else {
                    candidates.add(parentFqn + "." + name);
                }
            }
        }

        if (candidates.isEmpty()) {
            throw new AgiToolException("Member not found: " + memberFqn);
        }

        StringBuilder sb = new StringBuilder("Member not found: ").append(memberFqn);
        sb.append("\nDid you mean one of these canonical identification FQNs?\n");
        for (String c : candidates) {
            sb.append("- ").append(c).append("\n");
        }
        throw new AgiToolException(sb.toString());
    }

    /**
     * Calculates the insertion index for a new member based on the requested
     * relative position and an optional anchor member name.
     *
     * @param wc the working copy for element lookup
     * @param members the list of existing members in the parent tree
     * @param position the relative position (START, END, BEFORE, AFTER)
     * @param anchorMemberName the name of the member to anchor against
     * @return the 0-based insertion index
     * @throws AgiToolException if the anchor member is specified but not found
     */
    private int getInsertIndex(WorkingCopy wc, List<? extends Tree> members, RelativePosition position, String anchorMemberName) throws AgiToolException {
        int anchorIdx = anchorMemberName != null ? JavaSourceUtils.findMemberIndex(wc, members, anchorMemberName) : -1;
        if (anchorMemberName != null && anchorIdx == -1) {
            throw new AgiToolException("Anchor member not found: " + anchorMemberName);
        }
        return switch (position) {
            case START ->
                0;
            case END ->
                members.size();
            case BEFORE ->
                anchorIdx;
            case AFTER ->
                anchorIdx + 1;
        };
    }

    /**
     * Parses a string-based member body into a BlockTree. Ensures the body is
     * wrapped in curly braces if not already present.
     *
     * @param wc the working copy
     * @param body the raw body text
     * @return the parsed BlockTree node
     */
    private BlockTree parseBody(WorkingCopy wc, String body) {
        String b = body.trim().startsWith("{") ? body : "{" + body + "}";
        return (BlockTree) wc.getTreeUtilities().parseStatement(b, null);
    }

    /**
     * Logs import diagnostics directly to the tool's output stream. These
     * diagnostics typically include unresolved types and ranked candidates
     * found in the project index.
     *
     * @param diagnostics the set of diagnostic strings to log
     */
    private void logDiagnostics(Set<String> diagnostics) {
        if (!diagnostics.isEmpty()) {
            log("Import diagnostics:");
            diagnostics.forEach(d -> log(" - " + d));
        }
    }

}
