/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java;

import com.sun.source.tree.*;
import com.sun.source.util.TreePath;
import com.sun.source.util.TreePathScanner;
import java.io.OutputStream;
import java.util.*;
import java.util.stream.Collectors;
import javax.lang.model.element.*;
import javax.lang.model.type.TypeKind;
import javax.swing.text.Document;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.source.*;
import org.netbeans.api.java.source.support.ReferencesCount;
import org.netbeans.modules.editor.indent.api.Reformat;
import org.netbeans.modules.editor.java.Utilities;
import org.netbeans.modules.java.editor.base.imports.UnusedImports;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import uno.anahata.asi.agi.tool.*;
import uno.anahata.asi.nb.tools.java.JavaSourceUtils.RelativePosition;
import org.netbeans.api.java.source.TreePathHandle;

/**
 * V3.0.0 of the structural Java code refinement toolkit. High-precision
 * structural manipulation with a pragmatic approach to comment preservation.
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("AST-based updates of java code.")
public class CodeRefiner extends AnahataToolkit {

    @Override
    public void initialize() {
        getToolkit().setEnabled(false);
    }

    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList("CodeRefiner: The AST option for structural Java changes. Use when more appropiate/efficient than the Resources Toolkit."
                + "\n- 'declaration' must be the full signature (e.g., '@Override public void foo(int a) throws IOException'). It MUST include any annotations (e.g., @Override, @Deprecated) you want to preserve or add."
                + "\n- If 'declaration' is omitted in updateMember, the existing signature is preserved (Surgical Mode)."
                + "\n- 'body' is the content for the member: for methods/blocks, it's the logic inside the braces; for fields, it's the initializer expression. If omitted during update, the original content is preserved."
                + "\n- Javadoc must be updated using the specialized 'setJavadoc' tool. It is no longer a parameter of insert/update tools."
                + "\n- Identification Standard: 'memberFqn' must be ABSOLUTE (e.g. 'com.foo.MyClass.myField', 'com.foo.MyClass.myMethod(java.lang.String,int)', 'com.foo.MyClass$InnerClass.member', 'com.foo.MyClass.<clinit>#1()' or 'com.foo.MyClass.<init-block>#2()')."
                + "\n- METHODS/CONSTRUCTORS: Parentheses '()' are MANDATORY, even if there are no parameters (e.g. 'myMethod()')."
                + "\n- OVERLOADS: You MUST provide the canonical FQN of all parameters to disambiguate (e.g. 'java.lang.String,com.foo.MyClass$Inner')."
                + "\n- ARRAYS: Use '[]' for array parameters (e.g. 'java.lang.String[]', 'int[]')."
                + "\n- INITIALIZERS: Use <clinit>#n() for static blocks and <init-block>#n() for instance blocks, where n is the 1-based occurrence index in the class."
                + "\n- RULES FOR METHOD IDENTIFICATION: Parameter types must be CANONICAL: Fully Qualified, NO Generics, NO Annotations. Example: 'com.foo.Bar.process(java.util.List,int)'."
                + "\n- RelativePosition is MANDATORY for insertion/move. anchorMemberName is MANDATORY if position is BEFORE/AFTER."
                + "\n- ANCHORS: anchorMemberName must be RELATIVE to the class (e.g. 'myField', 'myMethod()', '<clinit>#1()' or '<init-block>#1()')."
                + "\n- records: insertMember and updateMember do not work with records due to a known nb bugs. Use the resources toolkit for records."
                + "\n- **DISCLAIMER**: Due to a known issue in the NetBeans AST parser, when inserting or updating the body of inner types (e.g. Classes, Enums, or Interfaces), internal comments within the members of the body of the inner type are lost. Javadocs are preserved. To add internal comments to nested types, use updateMember on individual members of the nested type or use the Resources toolkit. Otherwise, if you are inserting a new method or field, you can safely pass both declaration and body at once. ");
    }

    /**
     * Inserts a new member into a class.
     *
     * @param filePath the absolute path of the Java file
     * @param classFqn the FQN of the target class
     * @param declaration the full member declaration/signature
     * @param body the member body code
     * @param position position relative to anchor
     * @param anchorMemberName anchor member name relative to class
     * @param optimize whether to optimize imports
     * @param save whether to save the file
     * @return a success message
     * @throws Exception if insertion fails
     */
    @AgiTool("Inserts a new member into a class. Does not work with Records. When inserting innner types (like inner classess) the comments on members of the body of the inner class can be lost due to a known parsing issue). Other than that, if you are inserting a member that has a body, always include the body.")
    public String insertMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the target class. Use '$' for nested types (e.g. 'com.foo.Outer$Inner'). If empty, targets the file level.") String classFqn,
            @AgiToolParam(value = "The full member declaration/signature (e.g. '@Deprecated private String name;' or '@Override public void foo(String s) throws IOException'). It MUST include all annotations you wish to apply. Do NOT include the body code here.", rendererId = "java") String declaration,
            @AgiToolParam(value = "The WHOLE body code (logic inside the braces). Do NOT include the signature or the outer braces. This must be the entire content for the new member. Note: internal comments for nested types (classes/enums/interfaces) are lost.", rendererId = "java", required = false) String body,
            @AgiToolParam("Position relative to anchor.") RelativePosition position,
            @AgiToolParam(value = "Anchor member name relative to class (e.g. 'myField', 'myMethod()', 'foo(java.lang.String[])', '<clinit>#1()').", required = false) String anchorMemberName,
            @AgiToolParam(value = "Whether to optimize imports after insertion. Defaults to true.", required = false) Boolean optimize,
            @AgiToolParam("Whether to save the file.") boolean save) throws Exception {

        validatePosition(position, anchorMemberName);
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);
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
            JavaSourceUtils.handleSave(fo);
        }
        return "Inserted member into " + (classFqn == null || classFqn.isBlank() ? "file level" : classFqn);
    }

    /**
     * Updates an existing member structurally (Signature or Body).
     *
     * @param filePath the absolute path of the Java file
     * @param memberFqn the ABSOLUTE FQN of the member
     * @param declaration the new member declaration (signature)
     * @param body the new member body code
     * @param optimize whether to optimize imports
     * @param save whether to save the file
     * @return a success message
     * @throws Exception if update fails
     */
    @AgiTool("Updates an existing member structurally (Signature or Body). Does not work with Records."
            + " Use setJavadoc for Javadoc updates.")
    public String updateMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the member. Use 'package.Class.method(param1,param2)' or 'package.Class.method()' for no-arg methods. You MUST provide all parameter FQNs (e.g. 'java.lang.String,com.foo.MyClass$Inner'). Use 'package.Class$Inner' for types and 'package.Class.<clinit>#1()' for blocks.") String memberFqn,
            @AgiToolParam(value = "The new member declaration (signature). Do not provide it if it doesn't need to change. NEVER include the body here. If provided, it MUST include all annotations you wish to preserve (e.g., @Override, @Deprecated).", required = false, rendererId = "java") String declaration,
            @AgiToolParam(value = "The WHOLE body code (logic inside the braces for methods/blocks; for fields, it's the initializer expression). Do not provide it if it doesn't need to change. Do NOT include the signature or outer braces. Note: internal comments for nested types (classes/enums/interfaces) are lost.", rendererId = "java", required = false) String body,
            @AgiToolParam(value = "Whether to optimize imports after update. Defaults to true.", required = false) Boolean optimize,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);
        ModificationResult res = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            GeneratorUtilities gu = GeneratorUtilities.get(wc);
            Tree oldTree = JavaSourceUtils.findTree(wc, memberFqn);
            if (oldTree == null) {
                throwMemberNotFound(wc, memberFqn);
            }
            if (declaration != null || body != null) {
                Tree newTree;
                if (declaration == null) {
                    newTree = cloneTree(make, oldTree);
                    if (body != null) {
                        if (newTree instanceof MethodTree mt) {
                            String wrappedBody = body.trim().startsWith("{") ? body : "{" + body + "\n}";
                            newTree = make.Method(mt.getModifiers(), mt.getName(), mt.getReturnType(), mt.getTypeParameters(), mt.getParameters(), mt.getThrows(), make.createMethodBody((MethodTree) oldTree, wrappedBody), (AnnotationTree) mt.getDefaultValue());
                        } else if (newTree instanceof VariableTree vt) {
                            ExpressionTree finalInit = wc.getTreeUtilities().parseExpression(body, null);
                            newTree = make.Variable(vt.getModifiers(), vt.getName(), vt.getType(), finalInit);
                        } else if (newTree instanceof BlockTree bt) {
                            BlockTree parsed = parseBody(wc, body);
                            newTree = make.Block(parsed.getStatements(), bt.isStatic());
                        } else if (newTree instanceof ClassTree ct) {
                            Tree parsed = parseMember(wc, "class __Dummy", body);
                            if (parsed instanceof ClassTree parsedCt) {
                                List<Tree> newMembers = new ArrayList<>();
                                for (Tree m : parsedCt.getMembers()) {
                                    if (m instanceof MethodTree mt && mt.getName().contentEquals("<init>") && !body.contains(ct.getSimpleName().toString())) {
                                        continue;
                                    }
                                    newMembers.add(make.asNew(m));
                                }
                                newTree = make.asReplacementOf(rebuildClassTree(make, ct, newMembers), ct);
                            }
                        }
                    }
                } else {
                    newTree = parseMember(wc, declaration, body);
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
                make.asReplacementOf(newTree, oldTree);
                wc.rewrite(oldTree, newTree);
            }
        });
        res.commit();
        if (optimize == null || optimize) {
            optimizeImportsInternal(js, true);
        }
        if (save) {
            JavaSourceUtils.handleSave(fo);
        }
        return "Updated member " + memberFqn;
    }

    /**
     * Removes a member structurally.
     *
     * @param filePath the absolute path of the Java file
     * @param memberFqn the ABSOLUTE FQN of the member
     * @param optimize whether to optimize imports
     * @param save whether to save the file
     * @return a success message
     * @throws Exception if deletion fails
     */
    @AgiTool("Removes a member structurally.")
    public String deleteMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the member (e.g. 'package.Class.method(java.lang.String,int)', 'package.Class$Inner', 'package.Class.<clinit>#1()'). Parentheses '()' are mandatory for executables.") String memberFqn,
            @AgiToolParam(value = "Whether to optimize imports after deleting the member. Defaults to true.", required = false) Boolean optimize,
            @AgiToolParam("Whether to save the file.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);
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
            JavaSourceUtils.handleSave(fo);
        }
        return "Removed member '" + memberFqn + "' structurally.";
    }

    /**
     * Moves a member to a new position within the same class.
     *
     * @param filePath the absolute path of the Java file
     * @param memberFqn the ABSOLUTE FQN of the member
     * @param position position relative to anchor
     * @param anchorMemberName anchor member name relative to class
     * @param save whether to save the file
     * @return a success message
     * @throws Exception if move fails
     */
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
            // FIX: Use asReplacementOf to preserve Javadocs and comments of the class itself
            ClassTree updatedParent = make.asReplacementOf(rebuildClassTree(make, parentTree, members), parentTree);
            wc.rewrite(parentTree, updatedParent);
        }).commit();
        if (save) {
            JavaSourceUtils.handleSave(fo);
        }
        return "Moved member '" + memberFqn + "' to " + position;
    }

    /**
     * Adds one or more imports to a file structurally.
     *
     * @param filePath the absolute path of the Java file
     * @param imports list of FQNs to import
     * @param save whether to save the file
     * @return a success message
     * @throws Exception if import fails
     */
    @AgiTool("Adds one or more imports to a file structurally.")
    public String addImports(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("List of FQNs to import.") List<String> imports,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {
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
            JavaSourceUtils.handleSave(fo);
        }
        return "Added " + imports.size() + " import(s) to " + fo.getNameExt();
    }

    /**
     * Optimizes imports (converts FQNs to simple names, removes unused).
     *
     * @param filePath the absolute path of the Java file
     * @param removeUnused whether to remove unused imports
     * @param save whether to save the file
     * @return a success message
     * @throws Exception if optimization fails
     */
    @AgiTool("Optimizes imports (converts FQNs to simple names, removes unused).")
    public String optimizeImports(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam(value = "Whether to remove unused imports.", required = false) Boolean removeUnused,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        final boolean doRemove = removeUnused == null || removeUnused;
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);
        optimizeImportsInternal(js, doRemove);
        if (save) {
            JavaSourceUtils.handleSave(fo);
        }
        return "Optimized imports for: " + fo.getNameExt() + ". Check logs for details.";
    }

    /**
     * Reformats a file using IDE rules.
     *
     * @param filePath the absolute path of the Java file
     * @param save whether to save the file
     * @return a success message
     * @throws Exception if reformat fails
     */
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
            JavaSourceUtils.handleSave(fo);
        }
        return "Reformated: " + fo.getNameExt();
    }

    /**
     * Validates the consistency of the relative position and anchor member
     * name.
     *
     * @param position the relative position
     * @param anchor the anchor member name
     * @throws AgiToolException if validation fails
     */
    private static void validatePosition(RelativePosition position, String anchor) throws AgiToolException {
        if (position == null) {
            throw new AgiToolException("RelativePosition is mandatory.");
        }
        if ((position == RelativePosition.BEFORE || position == RelativePosition.AFTER) && (anchor == null || anchor.isBlank())) {
            throw new AgiToolException("anchorMemberName is mandatory for position " + position);
        }
    }

    /**
     * Rebuilds a ClassTree with a new list of members, preserving original
     * modifiers and name.
     *
     * @param make the tree maker
     * @param ct the original class tree
     * @param members the new member list
     * @return the reconstructed class tree
     */
    private static ClassTree rebuildClassTree(TreeMaker make, ClassTree ct, List<Tree> members) {
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
     * Parses a string-based member declaration and optional body into a Tree.
     *
     * @param wc the working copy
     * @param declaration the declaration string
     * @param body the body string
     * @return the parsed tree
     * @throws Exception if parsing fails
     */
    private static Tree parseMember(WorkingCopy wc, String declaration, String body) throws Exception {
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
     * Parses a raw body string into a BlockTree.
     *
     * @param wc the working copy
     * @param body the body string
     * @return the parsed block tree
     */
    private static BlockTree parseBody(WorkingCopy wc, String body) {
        String b = body.trim().startsWith("{") ? body : "{" + body + "}";
        return (BlockTree) wc.getTreeUtilities().parseStatement(b, null);
    }

    /**
     * Internal implementation of the import optimization logic.
     *
     * @param js the java source
     * @param removeUnused whether to remove unused imports
     * @throws Exception if optimization fails
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
                                CodeRefiner.this.log("Unresolved type: " + name);
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
                                        CodeRefiner.this.log("Found " + candidates.size() + " candidates for " + name + " (NetBeans Importance sort):");
                                        candidates.forEach(c -> CodeRefiner.this.log(" - " + c.fqn));
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
     * Surgically removes unused imports using the java editor base imports API.
     *
     * @param copy the working copy
     */
    private static void removeUnusedImportsInternal(WorkingCopy copy) {
        try {
            List<TreePathHandle> unused = UnusedImports.computeUnusedImports(copy);
            if (!unused.isEmpty()) {
                CompilationUnitTree cut = copy.getCompilationUnit();
                for (TreePathHandle handle : unused) {
                    TreePath path = handle.resolve(copy);
                    if (path != null && path.getLeaf() instanceof ImportTree it) {
                        cut = copy.getTreeMaker().removeCompUnitImport(cut, it);
                    }
                }
                copy.rewrite(copy.getCompilationUnit(), cut);
            }
        } catch (Exception e) {
            log.error("Failed to remove unused imports using UnusedImports utility", e);
        }
    }

    /**
     * Throws an AgiToolException when a requested member cannot be found.
     *
     * @param wc the working copy
     * @param memberFqn the FQN that was not found
     * @throws AgiToolException always
     */
    private static void throwMemberNotFound(WorkingCopy wc, String memberFqn) throws AgiToolException {
        throw new AgiToolException(JavaSourceUtils.getMemberNotFoundMessage(wc, memberFqn));
    }

    /**
     * Calculates the insertion index for a new member.
     *
     * @param wc the working copy
     * @param members the existing members
     * @param position the relative position
     * @param anchorMemberName the anchor name
     * @return the insertion index
     * @throws AgiToolException if anchor not found
     */
    private static int getInsertIndex(WorkingCopy wc, List<? extends Tree> members, RelativePosition position, String anchorMemberName) throws AgiToolException {
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
     * Clones a tree node for structural rewrite.
     *
     * @param make the tree maker
     * @param tree the tree to clone
     * @return the cloned tree
     */
    private static Tree cloneTree(TreeMaker make, Tree tree) {
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

}
