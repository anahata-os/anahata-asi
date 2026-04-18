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
import org.netbeans.api.java.source.support.ReferencesCount;
import org.netbeans.modules.editor.java.Utilities;

/**
 * V2.5 of the structural Java code refinement toolkit. High-fidelity structural
 * manipulation using full declarations and AST-based rewriting. Features
 * canonical member identification and surgical updates (optional signatures).
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Structural Java code refinement (V2). Uses full declarations for high-fidelity AST-based updates.")
public class CodeRefiner2 extends AnahataToolkit {

    @Override
    public void initialize() {
        getToolkit().setEnabled(true);
    }

    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList("CodeRefiner2: The authority for structural Java changes. For\u00e7a Bar\u00e7a!"
                + "\n- 'declaration' must be the full signature (e.g., '@Override public void foo(int a) throws IOException'). It MUST include any annotations (e.g., @Override, @Deprecated) you want to preserve or add."
                + "\n- If 'declaration' is null/omitted in updateMember, the existing signature is preserved (Surgical Mode)."
                + "\n- 'body' is just the logic inside the braces. For methods, if body is null or not given during update, the original body is preserved."
                + "\n- 'javadoc' content MUST NOT include the /** and */ markers. The tool adds them automatically."
                + "\n- Identification Standard: 'memberFqn' must be ABSOLUTE (e.g. 'com.foo.MyClass.myField', 'com.foo.MyClass.myMethod(java.lang.String,int)', 'com.foo.MyClass$InnerClass.member', 'com.foo.MyClass.<clinit>#1()' or 'com.foo.MyClass.<init-block>#2()')."
                + "\n- METHODS/CONSTRUCTORS: Parentheses '()' are MANDATORY, even if there are no parameters (e.g. 'myMethod()')."
                + "\n- OVERLOADS: You MUST provide the canonical FQN of all parameters to disambiguate (e.g. 'java.lang.String,com.foo.MyClass$Inner')."
                + "\n- ARRAYS: Use '[]' for array parameters (e.g. 'java.lang.String[]', 'int[]')."
                + "\n- INITIALIZERS: Use <clinit>#n() for static blocks and <init-block>#n() for instance blocks, where n is the 1-based occurrence index in the class."
                + "\n- RULES FOR METHOD IDENTIFICATION: Parameter types must be CANONICAL: Fully Qualified, NO Generics, NO Annotations. Example: 'com.foo.Bar.process(java.util.List,int)'."
                + "\n- RelativePosition is MANDATORY for insertion/move. anchorMemberName is MANDATORY if position is BEFORE/AFTER."
                + "\n- ANCHORS: anchorMemberName must be RELATIVE to the class (e.g. 'myField', 'myMethod()', '<clinit>#1()' or '<init-block>#1()').");
    }

    @AgiTool("Inserts a new member into a class.")
    public String insertMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the target class. Use '$' for nested types (e.g. 'com.foo.Outer$Inner').") String classFqn,
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
            TypeElement te = wc.getElements().getTypeElement(classFqn);
            if (te == null) {
                throw new AgiToolException("Class not found: " + classFqn);
            }
            ClassTree ct = (ClassTree) wc.getTrees().getTree(te);
            Tree newMember = parseMember(wc, declaration, body);
            applyJavadoc(wc, newMember, null, javadoc, true);
            newMember = GeneratorUtilities.get(wc).importFQNs(newMember);
            List<Tree> members = new ArrayList<>(ct.getMembers());
            int anchorIdx = anchorMemberName != null ? JavaSourceUtils.findMemberIndex(wc, members, anchorMemberName) : -1;
            if (anchorMemberName != null && anchorIdx == -1) {
                throw new AgiToolException("Anchor member not found: " + anchorMemberName);
            }
            int insertIdx = switch (position) {
                case START ->
                    0;
                case END ->
                    members.size();
                case BEFORE ->
                    anchorIdx;
                case AFTER ->
                    anchorIdx + 1;
            };
            members.add(insertIdx, newMember);
            wc.rewrite(ct, make.Class(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(), ct.getExtendsClause(), ct.getImplementsClause(), members));
            if (optimize == null || optimize) {
                optimizeImportsInternal(wc, true, diagnostics);
            }
        });
        res.commit();
        if (save) {
            handleSave(fo);
        }
        StringBuilder sb = new StringBuilder("Inserted member into ").append(classFqn);
        if (!diagnostics.isEmpty()) {
            sb.append(". Import diagnostics:\n");
            diagnostics.forEach(d -> sb.append(" - ").append(d).append("\n"));
        }
        return sb.toString();
    }

    @AgiTool("Updates an existing member structurally (Signature, Body, or Javadoc).")
    public String updateMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the member. Use 'package.Class.method(param1,param2)' or 'package.Class.method()' for no-arg methods. You MUST provide all parameter FQNs (e.g. 'java.lang.String,com.foo.MyClass$Inner'). Use 'package.Class$Inner' for types and 'package.Class.<clinit>#1()' for blocks.") String memberFqn,
            @AgiToolParam(value = "The new member declaration (signature). If null, existing signature is kept. NEVER include the body here. If provided, it MUST include all annotations you wish to preserve (e.g., @Override, @Deprecated).", required = false, rendererId = "java") String declaration,
            @AgiToolParam(value = "The WHOLE body code (logic inside the braces). Do NOT include the signature or outer braces. To surgically change a fragment, use the Resources toolkit instead.", rendererId = "java", required = false) String body,
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
            Tree newTree;
            if (declaration == null) {
                if (oldTree instanceof MethodTree mt) {
                    BlockTree finalBody = mt.getBody();
                    if (body != null) {
                        String b = body.trim().startsWith("{") ? body : "{" + body + "}";
                        finalBody = (BlockTree) wc.getTreeUtilities().parseStatement(b, null);
                    }
                    newTree = make.Method(mt.getModifiers(), mt.getName(), mt.getReturnType(), mt.getTypeParameters(), mt.getParameters(), mt.getThrows(), finalBody, (AnnotationTree) mt.getDefaultValue());
                } else if (oldTree instanceof ClassTree ct) {
                    newTree = make.Class(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(), ct.getExtendsClause(), ct.getImplementsClause(), ct.getMembers());
                } else if (oldTree instanceof VariableTree vt) {
                    newTree = make.Variable(vt.getModifiers(), vt.getName(), vt.getType(), vt.getInitializer());
                } else if (oldTree instanceof BlockTree bt) {
                    String b = body.trim().startsWith("{") ? body : "{" + body + "}";
                    newTree = wc.getTreeUtilities().parseStatement(b, null);
                } else {
                    throw new AgiToolException("Surgical mode (declaration=null) is not supported for " + oldTree.getKind());
                }
                applyJavadoc(wc, newTree, oldTree, javadoc, javadoc != null);
            } else {
                newTree = parseMember(wc, declaration, body);
                applyJavadoc(wc, newTree, oldTree, javadoc, javadoc != null);
            }
            make.asReplacementOf(newTree, oldTree, false);
            newTree = GeneratorUtilities.get(wc).importFQNs(newTree);
            wc.rewrite(oldTree, newTree);
            if (optimize == null || optimize) {
                optimizeImportsInternal(wc, true, diagnostics);
            }
        });
        res.commit();
        if (save) {
            handleSave(fo);
        }
        StringBuilder sb = new StringBuilder("Updated member ").append(memberFqn);
        if (!diagnostics.isEmpty()) {
            sb.append(". Import diagnostics:\n");
            diagnostics.forEach(d -> sb.append(" - ").append(d).append("\n"));
        }
        return sb.toString();
    }

    @AgiTool("Removes a member structurally.")
    public String deleteMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the member (e.g. 'package.Class.method(java.lang.String,int)', 'package.Class$Inner', 'package.Class.<clinit>#1()'). Parentheses '()' are mandatory for executables.") String memberFqn,
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

            Element element = wc.getTrees().getElement(TreePath.getPath(wc.getCompilationUnit(), memberTree));
            Element parentElement = element != null ? element.getEnclosingElement() : null;

            if (parentElement == null && memberTree instanceof ClassTree) {
                // Might be a top level class deletion or we need to find parent via AST
                // ... handle appropriately
            }

            if (parentElement instanceof TypeElement te) {
                ClassTree parentTree = (ClassTree) wc.getTrees().getTree(te);
                List<Tree> members = new ArrayList<>(parentTree.getMembers());
                members.remove(memberTree);
                ClassTree updatedParent = make.Class(parentTree.getModifiers(), parentTree.getSimpleName(), parentTree.getTypeParameters(),
                        parentTree.getExtendsClause(), parentTree.getImplementsClause(), members);
                wc.rewrite(parentTree, updatedParent);
            } else {
                CompilationUnitTree cut = wc.getCompilationUnit();
                List<Tree> types = new ArrayList<>(cut.getTypeDecls());
                types.remove(memberTree);
                CompilationUnitTree updatedCut = make.CompilationUnit(cut.getPackage(), cut.getImports(), types, cut.getSourceFile());
                wc.rewrite(cut, updatedCut);
            }
        }).commit();

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

            int anchorIdx = anchorMemberName != null ? JavaSourceUtils.findMemberIndex(wc, members, anchorMemberName) : -1;
            if (anchorMemberName != null && anchorIdx == -1) {
                throw new AgiToolException("Anchor member not found: " + anchorMemberName);
            }
            int insertIdx = switch (position) {
                case START ->
                    0;
                case END ->
                    members.size();
                case BEFORE ->
                    anchorIdx;
                case AFTER ->
                    anchorIdx + 1;
            };
            members.add(insertIdx, memberTree);

            ClassTree updatedParent = make.Class(parentTree.getModifiers(), parentTree.getSimpleName(), parentTree.getTypeParameters(), parentTree.getExtendsClause(), parentTree.getImplementsClause(), members);
            wc.rewrite(parentTree, updatedParent);
        }).commit();

        if (save) {
            handleSave(fo);
        }
        return "Moved member '" + memberFqn + "' to " + position;
    }

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
                boolean exists = currentImports.stream().anyMatch(i -> i.getQualifiedIdentifier().toString().equals(imp));
                if (!exists) {
                    currentImports.add(make.Import(make.Identifier(imp), false));
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

    @AgiTool("Reformats a file using IDE rules.")
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
            javax.swing.text.Document doc = wc.getDocument();
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

    /**
     * Optimizes imports (converts FQNs to simple names, removes unused).
     */
    @AgiTool("Optimizes imports (converts FQNs to simple names, removes unused).")
    public String optimizeImports(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath, @AgiToolParam(value = "Whether to remove unused imports.", required = false) Boolean removeUnused, @AgiToolParam("Whether to save.") boolean save) throws Exception {

        final boolean doRemove = removeUnused == null || removeUnused;
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);
        final Set<String> diagnostics = new LinkedHashSet<>();
        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            optimizeImportsInternal(wc, doRemove, diagnostics);

        }).commit();
        diagnostics.forEach(this::log);
        if (save) {
            handleSave(fo);
        }
        return "Optimized imports for: " + fo.getNameExt() + ". Check logs for details.";
    }

    /**
     * Internal logic for import optimization, reusable across structural tools.
     */
    private void optimizeImportsInternal(WorkingCopy wc, boolean removeUnused, Set<String> diagnostics) {
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
                            if (diagnostics.add("Unresolved type: " + name)) {
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
                                        diagnostics.add("Found " + candidates.size() + " candidates for " + name + " (NetBeans Importance sort):");
                                        candidates.forEach(c -> diagnostics.add(" - " + c.fqn));
                                    } else {
                                        diagnostics.add("No candidates found in index for " + name);
                                    }
                                } catch (Exception ex) {
                                    diagnostics.add("Error searching index for " + name + ": " + ex.getMessage());
                                    log.error("OptimizeImports: Index search failed for " + name, ex);
                                }
                            }
                        }
                    }
                }
                return super.visitIdentifier(node, wc);
            }
        }.scan(new TreePath(wc.getCompilationUnit()), wc);
    }

    private void validatePosition(RelativePosition position, String anchor) throws AgiToolException {
        if (position == null) {
            throw new AgiToolException("RelativePosition is mandatory.");
        }
        if ((position == RelativePosition.BEFORE || position == RelativePosition.AFTER) && (anchor == null || anchor.isBlank())) {
            throw new AgiToolException("anchorMemberName is mandatory for position " + position);
        }
    }

    private Tree parseMember(WorkingCopy wc, String declaration, String body) throws Exception {
        String decl = declaration.trim();
        if (!decl.endsWith(";") && !decl.endsWith("}")) {
            if (decl.contains("(")) {
                decl += " {}";
            } else {
                decl += ";";
            }
        }
        final String finalDecl = decl;
        String dummyClassName = "__Dummy";
        if (decl.contains("(") && !decl.contains(" ")) {
            String firstPart = decl.substring(0, decl.indexOf('(')).trim();
            if (firstPart.matches("[a-zA-Z0-9_]+")) {
                dummyClassName = firstPart;
            }
        }
        FileObject tempFo = FileUtil.createMemoryFileSystem().getRoot().createData(dummyClassName, "java");
        String dummyCode = "class " + dummyClassName + " { " + finalDecl + " }";
        try (OutputStream os = tempFo.getOutputStream()) {
            os.write(dummyCode.getBytes());
        }
        JavaSource js = JavaSource.forFileObject(tempFo);
        final Tree[] result = new Tree[1];
        js.runUserActionTask(cc -> {
            cc.toPhase(JavaSource.Phase.PARSED);
            CompilationUnitTree cut = cc.getCompilationUnit();
            if (!cut.getTypeDecls().isEmpty()) {
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
        }, true);
        return result[0];
    }

    private void applyJavadoc(WorkingCopy wc, Tree tree, Tree oldTree, String javadocText, boolean removeExisting) {
        TreeMaker make = wc.getTreeMaker();
        if (oldTree != null) {
            for (org.netbeans.api.java.source.Comment c : wc.getTreeUtilities().getComments(oldTree, true)) {
                if (c.isDocComment() && removeExisting) {
                    continue;
                }
                make.addComment(tree, c, true);
            }
        }
        if (javadocText != null && !javadocText.isBlank()) {
            String formatted = "/**\n * " + javadocText.replace("\n", "\n * ") + "\n */";
            make.addComment(tree, org.netbeans.api.java.source.Comment.create(org.netbeans.api.java.source.Comment.Style.JAVADOC, -1, -1, -1, formatted), true);
        }
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
            log.error("Failed to remove unused imports using HintsInvoker", e);
        }
    }

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
}
