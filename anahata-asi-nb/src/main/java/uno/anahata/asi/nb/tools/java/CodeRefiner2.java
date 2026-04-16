/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java;

import com.sun.source.tree.*;
import java.io.IOException;
import java.io.OutputStream;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.lang.model.element.*;
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

/**
 * V2.3 of the structural Java code refinement toolkit.
 * High-fidelity structural manipulation using full declarations and AST-based rewriting.
 * This toolkit is a "Clean Room" implementation maximizing reliance on NetBeans public APIs.
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
                + "\n- 'body' is just the logic inside the braces. For methods, if body is null during update, the original body is preserved."
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
            if (te == null) throw new AgiToolException("Class not found: " + classFqn);
            ClassTree ct = (ClassTree) wc.getTrees().getTree(te);

            Tree newMember = parseMember(wc, declaration, body);
            if (javadoc != null) applyJavadoc(wc, newMember, null, javadoc, true);
            newMember = GeneratorUtilities.get(wc).importFQNs(newMember);

            List<Tree> members = new ArrayList<>(ct.getMembers());
            int anchorIdx = anchorMemberName != null ? JavaSourceUtils.findMemberIndex(members, anchorMemberName) : -1;
            int insertIdx = switch (position) {
                case START -> 0;
                case END -> members.size();
                case BEFORE -> anchorIdx != -1 ? anchorIdx : 0;
                case AFTER -> anchorIdx != -1 ? anchorIdx + 1 : members.size();
            };
            
            members.add(insertIdx, newMember);
            wc.rewrite(ct, make.Class(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(), ct.getExtendsClause(), ct.getImplementsClause(), members));
        });

        res.commit();
        if (save) handleSave(fo);
        return "Inserted member into " + classFqn;
    }

    @AgiTool("Updates an existing member structurally using a new declaration.")
    public String updateMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the member to update.") String memberFqn,
            @AgiToolParam("The new member declaration/signature.") String declaration,
            @AgiToolParam(value = "Optional new body code. If null, existing body is kept.", rendererId = "java", required = false) String body,
            @AgiToolParam(value = "Optional new Javadoc.", required = false) String javadoc,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        ModificationResult res = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            Element element = JavaSourceUtils.findElement(wc, memberFqn);
            if (element == null) throw new AgiToolException("Member not found: " + memberFqn);
            Tree oldTree = wc.getTrees().getTree(element);

            Tree newTree = parseMember(wc, declaration, body);
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

            make.asReplacementOf(newTree, oldTree, false);
            applyJavadoc(wc, newTree, oldTree, javadoc, javadoc != null);
            newTree = GeneratorUtilities.get(wc).importFQNs(newTree);
            wc.rewrite(oldTree, newTree);
        });

        res.commit();
        if (save) handleSave(fo);
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
            if (element == null) throw new AgiToolException("Member not found: " + memberFqn);

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

        if (save) handleSave(fo);
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
            if (element == null) throw new AgiToolException("Member not found: " + memberFqn);
            
            Tree memberTree = wc.getTrees().getTree(element);
            Element parentElement = element.getEnclosingElement();
            if (!(parentElement instanceof TypeElement te)) throw new AgiToolException("Only members of a class can be moved.");
            
            ClassTree parentTree = (ClassTree) wc.getTrees().getTree(te);
            List<Tree> members = new ArrayList<>(parentTree.getMembers());
            members.remove(memberTree);
            
            int anchorIdx = anchorMemberName != null ? JavaSourceUtils.findMemberIndex(members, anchorMemberName) : -1;
            int insertIdx = switch (position) {
                case START -> 0;
                case END -> members.size();
                case BEFORE -> anchorIdx != -1 ? anchorIdx : 0;
                case AFTER -> anchorIdx != -1 ? anchorIdx + 1 : members.size();
            };
            members.add(insertIdx, memberTree);
            
            ClassTree updatedParent = make.Class(parentTree.getModifiers(), parentTree.getSimpleName(), parentTree.getTypeParameters(), parentTree.getExtendsClause(), parentTree.getImplementsClause(), members);
            wc.rewrite(parentTree, updatedParent);
        }).commit();

        if (save) handleSave(fo);
        return "Moved member '" + memberFqn + "' to " + position;
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

        if (save) handleSave(fo);
        return "Reformated: " + fo.getNameExt();
    }

    @AgiTool("Optimizes imports (converts FQNs to simple names, removes unused).")
    public String optimizeImports(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {
        
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            CompilationUnitTree cut = GeneratorUtilities.get(wc).importFQNs(wc.getCompilationUnit());
            wc.rewrite(wc.getCompilationUnit(), cut);
            removeUnusedImportsInternal(wc);
        }).commit();

        if (save) handleSave(fo);
        return "Optimized imports for: " + fo.getNameExt();
    }

    private void validatePosition(RelativePosition position, String anchor) throws AgiToolException {
        if (position == null) throw new AgiToolException("RelativePosition is mandatory.");
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
                if (decl.contains("(")) decl += " {}"; 
                else decl += ";";
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
                        if (member instanceof MethodTree mt && mt.getName().contentEquals("<init>")) continue;
                        result[0] = member;
                        break;
                    }
                }
            }, true);

            if (result[0] == null) throw new AgiToolException("Failed to parse: " + declaration);
            
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
        TreeUtilities utils = wc.getTreeUtilities();
        if (oldTree != null) {
            GeneratorUtilities.get(wc).copyComments(oldTree, tree, false);
            for (org.netbeans.api.java.source.Comment c : utils.getComments(oldTree, true)) {
                if (c.isDocComment() && (removeExisting || javadocText != null)) continue;
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
        if (ec != null && ec.getOpenedPanes() != null) ec.saveDocument();
        SaveCookie sc = doid.getLookup().lookup(SaveCookie.class);
        if (sc != null) sc.save();
        fo.refresh();
    }
}
