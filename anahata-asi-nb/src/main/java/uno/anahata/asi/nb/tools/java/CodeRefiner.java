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
import uno.anahata.asi.nb.tools.java.coderefiner.RelativePosition;
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
        getToolkit().setEnabled(true);
    }

    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList(JavaSourceUtils.CANONICAL_FQN_STANDARD
                + "\n"
                + "CodeRefiner: Maintenance toolkit for Java source files.\n"
                + "- **addImports**: Structurally adds new imports to the file.\n"
                + "- **optimizeImports**: Removes unused imports and converts FQNs to simple names where possible.\n"
                + "- **reformat**: Applies IDE formatting rules to the file (requires the file to be open in the editor).");
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
    @AgiTool("Adds one or more imports to a file structurally. Use this if adding imports is the only change you need to make in the file. If you need to change any other code also, use other toolkits to do it all in one go as this one would cause the lastModified to change if save is true (save=fales only works for files that are open in the editor).")
    public String addImports(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("List of FQNs to import.") List<String> imports,
            @AgiToolParam("Whether to save the changes, save=false would only work if the file is open in the editor.") boolean save) throws Exception {
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
     * Adds an annotation to a class, method, or field.
     *
     * @param filePath the absolute path of the Java file
     * @param memberFqn the ABSOLUTE FQN of the member
     * @param annotation the annotation to add
     * @param save whether to save
     * @return a success message
     * @throws Exception if it fails
     */
    @AgiTool("Adds an annotation to a class, method, or field.")
    public String addAnnotation(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the member (e.g. 'com.foo.Bar.myMethod(int)').") String memberFqn,
            @AgiToolParam("The annotation to add (e.g. '@Override' or '@SneakyThrows').") String annotation,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);
        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            Tree tree = JavaSourceUtils.findTree(wc, memberFqn);
            if (tree == null) {
                throw new AgiToolException("Member not found: " + memberFqn);
            }
            
            TreeMaker make = wc.getTreeMaker();
            ModifiersTree oldMods = null;
            if (tree instanceof MethodTree mt) {
                oldMods = mt.getModifiers();
            } else if (tree instanceof VariableTree vt) {
                oldMods = vt.getModifiers();
            } else if (tree instanceof ClassTree ct) {
                oldMods = ct.getModifiers();
            }
            
            if (oldMods != null) {
                String annName = annotation.startsWith("@") ? annotation.substring(1) : annotation;
                AnnotationTree annTree = make.Annotation(make.Identifier(annName), Collections.emptyList());
                ModifiersTree newMods = make.addModifiersAnnotation(oldMods, annTree);
                wc.rewrite(oldMods, newMods);
            }
        }).commit();
        if (save) {
            JavaSourceUtils.handleSave(fo);
        }
        return "Added annotation " + annotation + " to " + memberFqn;
    }

    /**
     * Sets or updates the Javadoc of a class, method, or field.
     *
     * @param filePath the absolute path of the Java file
     * @param memberFqn the ABSOLUTE FQN of the member
     * @param javadoc the Javadoc intent
     * @param save whether to save
     * @return a success message
     * @throws Exception if it fails
     */
    /*
    @AgiTool("Sets or updates the Javadoc of a class, method, or field.")
    public String setJavadoc(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the member.") String memberFqn,
            @AgiToolParam("The structured Javadoc definition.") uno.anahata.asi.nb.tools.java.coderefiner.JavadocIntent javadoc,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        uno.anahata.asi.nb.tools.java.coderefiner.CodeRefinementBatch batch = new uno.anahata.asi.nb.tools.java.coderefiner.CodeRefinementBatch();
        String uri = new java.io.File(filePath).toURI().toString();
        String uuid = getAgi().getResourceManager().getUuidByUri(uri);
        if (uuid == null) {
            throw new AgiToolException("File is not in the context window. Please load it first.");
        }
        batch.setResourceUuid(uuid);
        batch.setLastModified(getAgi().getResourceManager().get(uuid).getLastLoadTimestamp());
        batch.setSave(save);
        batch.setOptimize(true);
        
        uno.anahata.asi.nb.tools.java.coderefiner.CodeRefinementIntent intent = new uno.anahata.asi.nb.tools.java.coderefiner.CodeRefinementIntent();
        intent.setType(uno.anahata.asi.nb.tools.java.coderefiner.CodeRefinementIntent.Type.UPDATE);
        intent.setMemberFqn(memberFqn);
        intent.setJavadoc(javadoc);
        batch.setIntents(Collections.singletonList(intent));
        
        BatchCodeRefiner refiner = getToolkit(BatchCodeRefiner.class);
        return refiner.refine(batch);
    }
*/

}
