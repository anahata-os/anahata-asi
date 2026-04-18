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
import org.openide.filesystems.FileSystem;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import uno.anahata.asi.agi.tool.*;
import uno.anahata.asi.nb.tools.java.JavaSourceUtils.RelativePosition;
import org.netbeans.api.java.source.ClassIndex;
import org.netbeans.api.java.source.ElementHandle;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.ElementKind;
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
        return Collections.singletonList("CodeRefiner2: The authority for structural Java changes. Força Barça!"
                + "\n- 'declaration' must be the full signature (e.g., 'public void foo(int a) throws IOException')."
                + "\n- If 'declaration' is null/omitted in updateMember, the existing signature is preserved (Surgical Mode)."
                + "\n- 'body' is just the logic inside the braces. For methods, if body is null or not given during update, the original body is preserved."
                + "\n- 'javadoc' is what good java developers want in all members. To just add javadoc to an existing member do not pass 'declaration' or 'body'."
                + "\n- Identification Standard: 'package.Class.member' (Field) or 'package.Class.method(param1,param2)' (Method)."
                + "\n- RULES FOR METHOD IDENTIFICATION: Parameter types must be CANONICAL: Fully Qualified, NO Generics, NO Annotations. "
                + "Example: 'com.foo.Bar.process(java.util.List,int)'."
                + "\n- RelativePosition is MANDATORY for insertion/move. anchorMemberName is MANDATORY if position is BEFORE/AFTER.");
    }

    @AgiTool("Inserts a new member structurally into a class.")
    public String insertMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the target class.") String classFqn,
            @AgiToolParam("The full member declaration (e.g. 'private String name;' or 'public void foo(String s)').") String declaration,
            @AgiToolParam(value = "Optional body code (logic inside braces).", rendererId = "java", required = false) String body,
            @AgiToolParam(value = "The Javadoc content (without markers).", required = false) String javadoc,
            @AgiToolParam(value = "Anchor member name for positioning. Mandatory for BEFORE/AFTER.", required = false) String anchorMemberName,
            @AgiToolParam("Position relative to anchor. (START, END, BEFORE, AFTER)") RelativePosition position,
            @AgiToolParam("Whether to save the file.") boolean save) throws Exception {

        validatePosition(position, anchorMemberName);
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

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
        });

        res.commit();
        if (save) {
            handleSave(fo);
        }
        return "Inserted member into " + classFqn;
    }

    @AgiTool("Updates an existing member structurally using a new declaration.")
    public String updateMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the member to update.") String memberFqn,
            @AgiToolParam(value = "The new member declaration/signature. If null, existing signature is kept.", required = false) String declaration,
            @AgiToolParam(value = "Optional new body code. If null, existing body is kept.", rendererId = "java", required = false) String body,
            @AgiToolParam(value = "Optional new Javadoc.", required = false) String javadoc,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        ModificationResult res = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            Element element = JavaSourceUtils.findElement(wc, memberFqn);
            if (element == null) {
                throwMemberNotFound(wc, memberFqn);
            }

            Tree oldTree = wc.getTrees().getTree(element);
            Tree newTree;

            if (declaration == null) {
                if (oldTree instanceof MethodTree mt) {
                    BlockTree finalBody = mt.getBody();
                    if (body != null) {
                        String b = body.trim().startsWith("{") ? body : "{" + body + "}";
                        finalBody = (BlockTree) wc.getTreeUtilities().parseStatement(b, null);
                    }
                    newTree = make.Method(mt.getModifiers(), mt.getName(), mt.getReturnType(),
                            mt.getTypeParameters(), mt.getParameters(), mt.getThrows(),
                            finalBody, (AnnotationTree) mt.getDefaultValue());
                } else if (oldTree instanceof ClassTree ct) {
                    newTree = make.Class(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(),
                            ct.getExtendsClause(), ct.getImplementsClause(), ct.getMembers());
                } else if (oldTree instanceof VariableTree vt) {
                    newTree = make.Variable(vt.getModifiers(), vt.getName(), vt.getType(), vt.getInitializer());
                } else {
                    throw new AgiToolException("Surgical mode (declaration=null) is not supported for " + oldTree.getKind());
                }
                applyJavadoc(wc, newTree, oldTree, javadoc, javadoc != null);
            } else {
                newTree = parseMember(wc, declaration, body);
                if (oldTree instanceof MethodTree oldMt && newTree instanceof MethodTree newMt) {
                    if (body == null && (newMt.getBody() == null || newMt.getBody().getStatements().isEmpty())) {
                        newTree = make.Method(newMt.getModifiers(), newMt.getName(), newMt.getReturnType(),
                                newMt.getTypeParameters(), newMt.getParameters(), newMt.getThrows(),
                                oldMt.getBody(), (AnnotationTree) newMt.getDefaultValue());
                    }
                } else if (oldTree instanceof ClassTree oldCt && newTree instanceof ClassTree newCt) {
                    if (body == null && (newCt.getMembers() == null || newCt.getMembers().isEmpty())) {
                        newTree = make.Class(newCt.getModifiers(), newCt.getSimpleName(), newCt.getTypeParameters(),
                                newCt.getExtendsClause(), newCt.getImplementsClause(), oldCt.getMembers());
                    }
                }
                applyJavadoc(wc, newTree, oldTree, javadoc, javadoc != null);
            }

            make.asReplacementOf(newTree, oldTree, false);
            newTree = GeneratorUtilities.get(wc).importFQNs(newTree);
            wc.rewrite(oldTree, newTree);
        });

        res.commit();
        if (save) {
            handleSave(fo);
        }
        return "Updated member " + memberFqn;
    }

    @AgiTool("Removes a member structurally.")
    public String deleteMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the member to remove.") String memberFqn,
            @AgiToolParam("Whether to save the file.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            Element element = JavaSourceUtils.findElement(wc, memberFqn);
            if (element == null) {
                throwMemberNotFound(wc, memberFqn);
            }

            Tree memberTree = wc.getTrees().getTree(element);
            Element parentElement = element.getEnclosingElement();

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

    @AgiTool("Moves a member to a new position.")
    public String moveMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the member to move.") String memberFqn,
            @AgiToolParam(value = "Anchor member name. Mandatory for BEFORE/AFTER.", required = false) String anchorMemberName,
            @AgiToolParam("Position relative to anchor. (START, END, BEFORE, AFTER)") RelativePosition position,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        validatePosition(position, anchorMemberName);
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            Element element = JavaSourceUtils.findElement(wc, memberFqn);
            if (element == null) {
                throwMemberNotFound(wc, memberFqn);
            }

            Tree memberTree = wc.getTrees().getTree(element);
            Element parentElement = element.getEnclosingElement();
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
        int lastDot = memberFqn.contains("(") ? memberFqn.substring(0, memberFqn.indexOf("(")).lastIndexOf(".") : memberFqn.lastIndexOf(".");
        if (lastDot == -1) {
            throw new AgiToolException("Member not found: " + memberFqn);
        }

        String parentFqn = memberFqn.substring(0, lastDot);
        String name = memberFqn.contains("(") ? memberFqn.substring(lastDot + 1, memberFqn.indexOf("(")) : memberFqn.substring(lastDot + 1);

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
            ReferencesCount rc = ReferencesCount.get(wc.getClasspathInfo());
            CompilationUnitTree oldCut = wc
                    .getCompilationUnit();
            CompilationUnitTree newCut = GeneratorUtilities.get(wc).importFQNs(oldCut);
            wc.rewrite(oldCut, newCut);
            if (doRemove) {
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

        }).commit();
        diagnostics.forEach(this::log);
        if (save) {
            handleSave(fo);
        }
        return "Optimized imports for: " + fo.getNameExt() + ". Check logs for details.";
    }

    private void validatePosition(RelativePosition position, String anchor) throws AgiToolException {
        if (position == null) {
            throw new AgiToolException("RelativePosition is mandatory.");
        }
        if ((position == RelativePosition.BEFORE || position == RelativePosition.AFTER) && (anchor == null || anchor.isBlank())) {
            throw new AgiToolException("anchorMemberName is mandatory for position " + position);
        }
    }

    private Tree parseMember(WorkingCopy wc, String declaration, String body) throws AgiToolException {
        try {
            FileSystem fs = FileUtil.createMemoryFileSystem();
            FileObject fo = fs.getRoot().createData("Dummy.java");

            String decl = declaration.trim();
            if (!decl.endsWith(";") && !decl.endsWith("}")) {
                if (decl.contains("(")) {
                    decl += " {}";
                } else {
                    decl += ";";
                }
            }

            String dummyCode = "class __Dummy { " + decl + " }";
            try (OutputStream os = fo.getOutputStream()) {
                os.write(dummyCode.getBytes());
            }

            JavaSource js = JavaSource.forFileObject(fo);
            final Tree[] result = new Tree[1];
            js.runUserActionTask(cc -> {
                cc.toPhase(JavaSource.Phase.PARSED);
                CompilationUnitTree cut = cc.getCompilationUnit();
                if (!cut.getTypeDecls().isEmpty()) {
                    ClassTree ct = (ClassTree) cut.getTypeDecls().get(0);
                    for (Tree member : ct.getMembers()) {
                        if (member instanceof MethodTree mt && mt.getName().contentEquals("<init>")) {
                            continue;
                        }
                        result[0] = member;
                        break;
                    }
                }
            }, true);

            if (result[0] == null) {
                throw new AgiToolException("Failed to parse: " + declaration);
            }

            Tree member = result[0];
            if (body != null && member instanceof MethodTree mt) {
                String b = body.trim().startsWith("{") ? body : "{" + body + "}";
                member = wc.getTreeMaker().Method(mt.getModifiers(), mt.getName(), mt.getReturnType(),
                        mt.getTypeParameters(), mt.getParameters(), mt.getThrows(),
                        (BlockTree) wc.getTreeUtilities().parseStatement(b, null),
                        (AnnotationTree) mt.getDefaultValue());
            }
            return member;
        } catch (IOException ex) {
            throw new AgiToolException("Parsing infrastructure failure", ex);
        }
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
