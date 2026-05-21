/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java;

import com.sun.source.tree.*;
import com.sun.source.util.SourcePositions;
import com.sun.source.util.TreePath;
import com.sun.source.util.TreePathScanner;
import java.util.*;
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
import org.openide.loaders.DataObject;
import uno.anahata.asi.agi.tool.*;
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
    @AgiTool("Optimizes imports (converts FQNs to simple names, removes unused and adds missing if not ambiguous).")
    public String optimizeImports(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam(value = "Whether to remove unused imports.", required = false) Boolean removeUnused,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        final boolean doRemove = removeUnused == null || removeUnused;
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        optimizeImportsInternal(fo, doRemove);
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
     * @param fo the file object
     * @param removeUnused whether to remove unused imports
     * @throws Exception if optimization fails
     */
    private void optimizeImportsInternal(FileObject fo, boolean removeUnused) throws Exception {
                JavaSource js1 = JavaSource.forFileObject(fo);
                js1.runModificationTask(wc -> {
                    wc.toPhase(JavaSource.Phase.RESOLVED);
                    ReferencesCount rc = ReferencesCount.get(wc.getClasspathInfo());
                    CompilationUnitTree originalCut = wc.getCompilationUnit();

                    final Set<TypeElement> importsToAdd = new LinkedHashSet<>();

                    new TreePathScanner<Void, WorkingCopy>() {
                        @Override
                        public Void visitPackage(PackageTree node, WorkingCopy wcSub) {
                            return null;
                        }

                        @Override
                        public Void visitImport(ImportTree node, WorkingCopy wcSub) {
                            return null;
                        }

                        @Override
                        public Void visitMemberSelect(MemberSelectTree node, WorkingCopy wcSub) {
                            SourcePositions sp = wcSub.getTrees().getSourcePositions();
                            if (sp.getStartPosition(originalCut, node) == -1) {
                                return super.visitMemberSelect(node, wcSub);
                            }
                            TreePath path = getCurrentPath();
                            if (path != null) {
                                Element e = wcSub.getTrees().getElement(path);
                                if (e instanceof TypeElement te) {
                                    String fqn = te.getQualifiedName().toString();
                                    String nodeStr = node.toString();
                                    if (nodeStr.equals(fqn)) {
                                        TypeElement outerType = null;
                                        Element enclosing = te.getEnclosingElement();
                                        while (enclosing instanceof TypeElement parentType) {
                                            outerType = parentType;
                                            enclosing = parentType.getEnclosingElement();
                                        }
                                        
                                        PackageElement tePkg = wcSub.getElements().getPackageOf(outerType != null ? outerType : te);
                                        String pkgName = tePkg != null ? tePkg.getQualifiedName().toString() : "";
                                        String currentPkg = originalCut.getPackage() != null ? originalCut.getPackage().getPackageName().toString() : "";
                                        boolean isImplicit = "java.lang".equals(pkgName) || currentPkg.equals(pkgName);

                                        if (!isImplicit) {
                                            if (outerType != null) {
                                                importsToAdd.add(outerType);
                                            } else {
                                                importsToAdd.add(te);
                                            }
                                        }
                                    }
                                }
                            }
                            return super.visitMemberSelect(node, wcSub);
                        }

                        @Override
                        public Void visitIdentifier(IdentifierTree node, WorkingCopy wcSub) {
                            TreePath path = getCurrentPath();
                            if (path != null) {
                                Element e = wcSub.getTrees().getElement(path);
                                if (e == null || (e.asType() != null && e.asType().getKind() == TypeKind.ERROR)) {
                                    String name = node.getName().toString();
                                    if (Character.isUpperCase(name.charAt(0))) {
                                        try {
                                            ClassIndex index = wcSub.getClasspathInfo().getClassIndex();
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
                                                    TypeElement te = handle.resolve(wcSub);
                                                    int score = te != null ? Utilities.getImportanceLevel(wcSub, rc, te) : Utilities.getImportanceLevel(handle.getQualifiedName());
                                                    candidates.add(new Candidate(handle.getQualifiedName(), score));
                                                }

                                                candidates.sort((c1, c2) -> Integer.compare(c2.score, c1.score));

                                                if (candidates.size() == 1) {
                                                    String bestFqn = candidates.get(0).fqn;
                                                    CodeRefiner.this.log("Auto-resolving single candidate import for '" + name + "': " + bestFqn);
                                                    TypeElement te = wcSub.getElements().getTypeElement(bestFqn);
                                                    if (te != null) {
                                                        TypeElement outerType = null;
                                                        Element enclosing = te.getEnclosingElement();
                                                        while (enclosing instanceof TypeElement parentType) {
                                                            outerType = parentType;
                                                            enclosing = parentType.getEnclosingElement();
                                                        }
                                                        if (outerType != null) {
                                                            importsToAdd.add(outerType);
                                                        } else {
                                                            importsToAdd.add(te);
                                                        }
                                                    }
                                                } else if (candidates.size() > 1) {
                                                    StringBuilder sb = new StringBuilder();
                                                    sb.append("Ambiguous candidates found for '").append(name).append("'. Skipping auto-import:\n");
                                                    for (Candidate cand : candidates) {
                                                        sb.append("  - ").append(cand.fqn).append(" (Importance Score: ").append(cand.score).append(")\n");
                                                    }
                                                    CodeRefiner.this.log(sb.toString());
                                                }
                                            }
                                        } catch (Exception ex) {
                                            log.error("OptimizeImports: Index search failed for " + name, ex);
                                        }
                                    }
                                }
                            }
                            return super.visitIdentifier(node, wcSub);
                        }
                    }.scan(new TreePath(originalCut), wc);

                    CompilationUnitTree evolvingCut = originalCut;
                    if (removeUnused) {
                        try {
                            List<TreePathHandle> unused = UnusedImports.computeUnusedImports(wc);
                            if (!unused.isEmpty()) {
                                for (TreePathHandle handle : unused) {
                                    TreePath path = handle.resolve(wc);
                                    if (path != null && path.getLeaf() instanceof ImportTree it) {
                                        evolvingCut = wc.getTreeMaker().removeCompUnitImport(evolvingCut, it);
                                    }
                                }
                            }
                        } catch (Exception e) {
                            log.error("Failed to remove unused imports", e);
                        }
                    }

                    if (!importsToAdd.isEmpty()) {
                        evolvingCut = GeneratorUtilities.get(wc).addImports(evolvingCut, importsToAdd);
                    }

                    if (originalCut != evolvingCut) {
                        wc.rewrite(originalCut, evolvingCut);
                    }
                }).commit();

                JavaSourceUtils.handleSave(fo);
                fo.refresh();

                // CRITICAL BUG FIX: Force a synchronous re-parse of the file so that NetBeans
                // updates its internally cached character offsets for the next transaction.
                // Without this, the second transaction is executed with stale offsets, causing
                // the FQN shortening rewrite to corrupt/duplicate the file.
                js1.runUserActionTask(cc -> {
                    cc.toPhase(JavaSource.Phase.RESOLVED);
                }, true);

                JavaSource js2 = JavaSource.forFileObject(fo);
                js2.runModificationTask(wc -> {
                    wc.toPhase(JavaSource.Phase.RESOLVED);
                    CompilationUnitTree cut = wc.getCompilationUnit();
                    TreeMaker make = wc.getTreeMaker();

                    new TreePathScanner<Void, WorkingCopy>() {
                        @Override
                        public Void visitPackage(PackageTree node, WorkingCopy wcSub) {
                            return null;
                        }

                        @Override
                        public Void visitImport(ImportTree node, WorkingCopy wcSub) {
                            return null;
                        }

                        @Override
                        public Void visitMemberSelect(MemberSelectTree node, WorkingCopy wcSub) {
                            SourcePositions sp = wcSub.getTrees().getSourcePositions();
                            if (sp.getStartPosition(cut, node) == -1) {
                                return super.visitMemberSelect(node, wcSub);
                            }
                            TreePath path = getCurrentPath();
                            if (path != null) {
                                Element e = wcSub.getTrees().getElement(path);
                                if (e instanceof TypeElement te) {
                                    String fqn = te.getQualifiedName().toString();
                                    String nodeStr = node.toString();
                                    if (nodeStr.equals(fqn)) {
                                        TypeElement outerType = null;
                                        Element enclosing = te.getEnclosingElement();
                                        while (enclosing instanceof TypeElement parentType) {
                                            outerType = parentType;
                                            enclosing = parentType.getEnclosingElement();
                                        }
                                        
                                        PackageElement tePkg = wcSub.getElements().getPackageOf(outerType != null ? outerType : te);
                                        String pkgName = tePkg != null ? tePkg.getQualifiedName().toString() : "";
                                        String currentPkg = cut.getPackage() != null ? cut.getPackage().getPackageName().toString() : "";
                                        boolean isImplicit = "java.lang".equals(pkgName) || currentPkg.equals(pkgName);

                                        boolean isImported = false;
                                        if (isImplicit) {
                                            isImported = true;
                                        } else {
                                            String targetFqn = outerType != null ? outerType.getQualifiedName().toString() : fqn;
                                            for (ImportTree imp : cut.getImports()) {
                                                String impStr = imp.getQualifiedIdentifier().toString();
                                                if (impStr.equals(targetFqn)) {
                                                    isImported = true;
                                                    break;
                                                }
                                            }
                                        }
                                        if (isImported) {
                                            String replacementName;
                                            if (outerType != null) {
                                                String pkg = wcSub.getElements().getPackageOf(outerType).getQualifiedName().toString();
                                                if (pkg.isEmpty()) {
                                                    replacementName = fqn;
                                                } else {
                                                    replacementName = fqn.substring(pkg.length() + 1);
                                                }
                                            } else {
                                                replacementName = te.getSimpleName().toString();
                                            }
                                            wcSub.rewrite(node, make.Identifier(replacementName));
                                        }
                                    }
                                }
                            }
                            return super.visitMemberSelect(node, wcSub);
                        }
                    }.scan(new TreePath(cut), wc);
                }).commit();
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

}
