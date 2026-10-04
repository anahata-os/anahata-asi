/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.components;

import java.io.File;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.Elements;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.source.ClasspathInfo;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.java.source.JavaSource.Phase;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.modules.java.source.indexing.JavaIndex;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.Opcodes;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.nb.tools.project.components.ProjectStructure.ScanStrategy;
import uno.anahata.asi.ide.tools.project.context.AbstractProjectContextProvider;
import uno.anahata.asi.ide.tools.project.ProjectStructureScope;

/**
 * A specialized container for a Java source group (e.g., src/main/java).
 * <p>
 * This class performs a deep scan of the Java source root, resolving logical 
 * types into a hierarchical package-centric view. It performs a hybrid scan, 
 * merging logical type information from the index with a physical filesystem 
 * walk to ensure 'package-info.java' and other non-indexed files are included.
 * </p>
 * 
 * @author Anahata
 */
@Slf4j
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public final class JavaSourceGroup extends ProjectNode {

    /** 
     * The display name of the source group. 
     */
    private String name;
    
    /** 
     * The physical path relative to the project root. 
     */
    private String relPath;

    /** 
     * The list of logical packages discovered within this group. 
     */
    @Builder.Default
    private List<JavaPackage> packages = new ArrayList<>();

    /**
     * The scan strategy used for this group.
     */
    private ScanStrategy scanStrategy;

    /**
     * Constructs a JavaSourceGroup with default warning tracking and strategy determined by scope.
     *
     * @param project The parent project.
     * @param sg The NetBeans source group instance.
     * @param scope The active project structure granularity scope.
     * @throws Exception if filesystem traversal or metadata resolution fails.
     */
    public JavaSourceGroup(Project project, SourceGroup sg, ProjectStructureScope scope) throws Exception {
        this(project, sg, scope, new ArrayList<>(), (scope != null && scope.isShowJavadoc()) ? ScanStrategy.JAVASOURCE_AST : ScanStrategy.ASM_SIG);
    }

    /**
     * Constructs and populates the Java source group logic using direct filesystem traversal and fast metadata resolution.
     * <p>
     * Implementation details:
     * 1. Walks the physical source root directly, grouping source files into {@link JavaPackage}s.
     * 2. Resolves metadata (element kinds, supertypes, inner classes, and Javadocs) using the chosen strategy:
     *    - Fast {@link ScanStrategy#ASM_SIG} via OW2 ASM reading NetBeans index cache (.sig files).
     *    - Deep {@link ScanStrategy#JAVASOURCE_AST} via a single-pass {@link JavaSource} compiler task when Javadoc is enabled.
     * </p>
     * 
     * @param project The parent project.
     * @param sg The NetBeans source group instance.
     * @param scope The active project structure granularity scope.
     * @param scanWarnings List to collect any resolution warnings.
     * @param scanStrategy The scan strategy to apply.
     * @throws Exception if filesystem operations fail.
     */
    public JavaSourceGroup(Project project, SourceGroup sg, ProjectStructureScope scope, List<String> scanWarnings, ScanStrategy scanStrategy) throws Exception {
        this.name = sg.getDisplayName();
        this.relPath = FileUtil.getRelativePath(project.getProjectDirectory(), sg.getRootFolder());
        this.packages = new ArrayList<>();
        this.scanStrategy = scanStrategy;

        FileObject root = sg.getRootFolder();
        Map<String, JavaPackage> pkgMap = new TreeMap<>();
        List<ProjectComponent> javaComponents = new ArrayList<>();

        // 1. Walk directory tree directly (packages and files)
        walkJavaPackages(root, root, pkgMap, javaComponents, scope);
        this.packages.addAll(pkgMap.values());

        // 2. Resolve metadata (element kind, supertypes, inner classes, javadoc) using selected strategy
        if (scanStrategy == ScanStrategy.JAVASOURCE_AST) {
            ClasspathInfo cpInfo = ClasspathInfo.create(root);
            resolveWithJavaSource(cpInfo, javaComponents, scope, scanWarnings != null ? scanWarnings : new ArrayList<>());
        } else {
            resolveWithAsm(root, javaComponents, scope, scanWarnings != null ? scanWarnings : new ArrayList<>());
        }
    }

    /**
     * Resolves metadata via fast OW2 ASM bytecode signature scanning from the NetBeans index cache (.sig files).
     *
     * @param root The source root folder.
     * @param components The list of primary top-level Java components to enrich.
     * @param scope The active project structure granularity scope.
     * @param scanWarnings List to record any warnings.
     */
    private void resolveWithAsm(FileObject root, List<ProjectComponent> components, ProjectStructureScope scope, List<String> scanWarnings) {
        File classFolder = null;
        try {
            classFolder = JavaIndex.getClassFolder(root.toURL(), true);
        } catch (Exception e) {
            log.debug("Could not resolve classFolder for root: {}", root.toURL(), e);
        }

        List<ProjectComponent> unindexed = new ArrayList<>();
        for (ProjectComponent comp : components) {
            String fqn = comp.getFqn();
            if (fqn == null) {
                continue;
            }

            boolean resolved = false;
            if (classFolder != null && classFolder.exists()) {
                String relSigPath = fqn.replace('.', '/') + ".sig";
                File sigFile = new File(classFolder, relSigPath);
                if (sigFile.exists()) {
                    try {
                        byte[] bytes = Files.readAllBytes(sigFile.toPath());
                        ClassReader cr = new ClassReader(bytes);
                        comp.setKind(resolveElementKind(cr.getAccess()));

                        if (scope.isShowSupertypes()) {
                            comp.setSupertypes(extractAsmSupertypes(cr));
                        }

                        if (scope.isShowInnerClasses()) {
                            resolveAsmInnerClasses(sigFile, comp, scope);
                        }
                        resolved = true;
                    } catch (Throwable t) {
                        log.warn("ASM failed to read .sig for {}: {}", fqn, t.getMessage(), t);
                        scanWarnings.add("`" + comp.getSimpleName() + "`: Failed to read index signature: " + t.getMessage());
                    }
                }
            }

            if (!resolved) {
                unindexed.add(comp);
            }
        }

        // Fallback to single-pass JavaSource only for unindexed files (if any)
        if (!unindexed.isEmpty()) {
            try {
                ClasspathInfo cpInfo = ClasspathInfo.create(root);
                resolveWithJavaSource(cpInfo, unindexed, scope, scanWarnings);
            } catch (Throwable t) {
                log.warn("JavaSource fallback failed for unindexed files in {}: {}", root.getName(), t.getMessage(), t);
            }
        }
    }

    /**
     * Resolves and attaches nested/inner classes using ASM .sig signature files.
     *
     * @param sigFile The parent class .sig file.
     * @param parentComp The parent project component.
     * @param scope The active project structure granularity scope.
     */
    private void resolveAsmInnerClasses(File sigFile, ProjectComponent parentComp, ProjectStructureScope scope) {
        File parentDir = sigFile.getParentFile();
        if (parentDir == null || !parentDir.exists()) {
            return;
        }

        String typeName = sigFile.getName().substring(0, sigFile.getName().length() - 4);
        String prefix = typeName + "$";
        File[] innerFiles = parentDir.listFiles((dir, name) -> name.startsWith(prefix) && name.endsWith(".sig"));
        if (innerFiles == null || innerFiles.length == 0) {
            return;
        }

        Map<String, ProjectComponent> innerMap = new HashMap<>();
        for (File innerFile : innerFiles) {
            try {
                byte[] innerBytes = Files.readAllBytes(innerFile.toPath());
                ClassReader innerCr = new ClassReader(innerBytes);
                String innerFqn = innerCr.getClassName().replace('/', '.');
                ElementKind innerKind = resolveElementKind(innerCr.getAccess());
                String innerSupertypes = scope.isShowSupertypes() ? extractAsmSupertypes(innerCr) : null;

                ProjectComponent innerComp = new ProjectComponent(parentComp.getFileObject(), null);
                innerComp.setFqn(innerFqn);
                innerComp.setKind(innerKind);
                innerComp.setSupertypes(innerSupertypes);
                innerMap.put(innerFqn, innerComp);
            } catch (Throwable t) {
                log.debug("Failed to read inner class .sig file {}: {}", innerFile.getName(), t.getMessage());
            }
        }

        for (ProjectComponent inner : innerMap.values()) {
            String innerFqn = inner.getFqn();
            int lastDollar = innerFqn.lastIndexOf('$');
            if (lastDollar != -1) {
                String outerFqn = innerFqn.substring(0, lastDollar);
                if (innerMap.containsKey(outerFqn)) {
                    innerMap.get(outerFqn).addChild(inner);
                } else {
                    parentComp.addChild(inner);
                }
            } else {
                parentComp.addChild(inner);
            }
        }
    }

    /**
     * Determines the Java ElementKind from class access flags.
     *
     * @param access The class access flags from ASM.
     * @return The corresponding ElementKind.
     */
    private static ElementKind resolveElementKind(int access) {
        if ((access & Opcodes.ACC_ANNOTATION) != 0) {
            return ElementKind.ANNOTATION_TYPE;
        }
        if ((access & Opcodes.ACC_INTERFACE) != 0) {
            return ElementKind.INTERFACE;
        }
        if ((access & Opcodes.ACC_ENUM) != 0) {
            return ElementKind.ENUM;
        }
        if ((access & Opcodes.ACC_RECORD) != 0) {
            return ElementKind.RECORD;
        }
        return ElementKind.CLASS;
    }

    /**
     * Extracts supertypes (extends and implements clauses) from an ASM ClassReader.
     *
     * @param cr The ASM ClassReader.
     * @return Formatted supertypes string, or null if none.
     */
    private static String extractAsmSupertypes(ClassReader cr) {
        String superName = cr.getSuperName();
        String[] ifaces = cr.getInterfaces();
        StringBuilder sb = new StringBuilder();
        if (superName != null 
                && !"java/lang/Object".equals(superName) 
                && !"java/lang/Enum".equals(superName) 
                && !"java/lang/Record".equals(superName)) {
            sb.append("extends ").append(simpleTypeName(superName.replace('/', '.')));
        }
        if (ifaces != null && ifaces.length > 0) {
            if (sb.length() > 0) {
                sb.append(" ");
            }
            sb.append("implements ");
            for (int i = 0; i < ifaces.length; i++) {
                if (i > 0) {
                    sb.append(", ");
                }
                sb.append(simpleTypeName(ifaces[i].replace('/', '.')));
            }
        }
        return sb.length() > 0 ? sb.toString() : null;
    }

    /**
     * Resolves metadata and class-level Javadoc comments in a fast, single-pass JavaSource task,
     * leveraging {@link CachedAstMetadata} to avoid re-parsing unchanged files across turns.
     *
     * @param cpInfo The classpath info for the source root.
     * @param components The collection of project components to enrich.
     * @param scope The active project structure granularity scope.
     * @param scanWarnings List to collect any resolution warnings.
     */
    private void resolveWithJavaSource(ClasspathInfo cpInfo, Collection<ProjectComponent> components, ProjectStructureScope scope, List<String> scanWarnings) {
        try {
            Map<FileObject, List<ProjectComponent>> fileToComponents = new HashMap<>();
            for (ProjectComponent comp : components) {
                FileObject fo = comp.getFileObject();
                if (fo.isValid()) {
                    CachedAstMetadata cached = CachedAstMetadata.get(fo);
                    if (cached != null) {
                        CachedAstMetadata.apply(comp, cached, scope);
                    } else {
                        fileToComponents.computeIfAbsent(fo, k -> new ArrayList<>()).add(comp);
                    }
                }
            }

            if (fileToComponents.isEmpty()) {
                return;
            }

            List<FileObject> files = new ArrayList<>(fileToComponents.keySet());
            JavaSource js = files.isEmpty() ? JavaSource.create(cpInfo) : JavaSource.create(cpInfo, files);
            if (js != null) {
                js.runUserActionTask(controller -> {
                    controller.toPhase(Phase.ELEMENTS_RESOLVED);
                    Elements elements = controller.getElements();
                    FileObject fo = controller.getFileObject();
                    List<ProjectComponent> compList = (fo != null) ? fileToComponents.get(fo) : null;
                    if (compList == null) {
                        return;
                    }

                    for (ProjectComponent comp : compList) {
                        String fqn = comp.getFqn();
                        if (fqn == null) {
                            continue;
                        }
                        TypeElement te = elements.getTypeElement(fqn);
                        if (te != null) {
                            populateTypeMetadata(comp, te, elements, scope, scanWarnings);
                            if (scope.isShowInnerClasses()) {
                                comp.getChildren().clear();
                                populateInnerClassesFromAst(comp, te, elements, scope, scanWarnings);
                            }
                            CachedAstMetadata.put(fo, comp);
                        }
                    }
                }, true);
            }
        } catch (Throwable e) {
            log.warn("Batch JavaSource AST metadata resolution failed: {}", e.getMessage(), e);
            scanWarnings.add("Batch AST resolution failed: " + e.getMessage());
        }
    }

    /**
     * Recursively populates nested inner classes discovered from the Javac AST.
     *
     * @param parentComp The parent project component.
     * @param te The enclosing TypeElement.
     * @param elements The javac Elements utility.
     * @param scope The active granularity scope.
     * @param scanWarnings List to record any warnings.
     */
    private void populateInnerClassesFromAst(ProjectComponent parentComp, TypeElement te, Elements elements, ProjectStructureScope scope, List<String> scanWarnings) {
        for (Element enclosed : te.getEnclosedElements()) {
            if (enclosed instanceof TypeElement innerTe) {
                try {
                    ProjectComponent innerComp = new ProjectComponent(parentComp.getFileObject(), null);
                    innerComp.setFqn(innerTe.getQualifiedName().toString());
                    populateTypeMetadata(innerComp, innerTe, elements, scope, scanWarnings);
                    parentComp.addChild(innerComp);
                    populateInnerClassesFromAst(innerComp, innerTe, elements, scope, scanWarnings);
                } catch (Throwable t) {
                    log.debug("Failed to create inner class component for {}: {}", innerTe.getQualifiedName(), t.getMessage());
                }
            }
        }
    }

    /**
     * Extracts element kind, supertypes, and class-level Javadoc comments from a resolved TypeElement.
     *
     * @param comp The project component to populate.
     * @param te The resolved TypeElement symbol.
     * @param elements The javac Elements utility.
     * @param scope The active granularity scope.
     * @param scanWarnings List to record any exceptions.
     */
    private void populateTypeMetadata(ProjectComponent comp, TypeElement te, Elements elements, ProjectStructureScope scope, List<String> scanWarnings) {
        comp.setKind(te.getKind());
        if (scope.isShowSupertypes()) {
            try {
                StringBuilder stSb = new StringBuilder();
                TypeMirror superclass = te.getSuperclass();
                if (superclass != null 
                        && superclass.getKind() != TypeKind.NONE 
                        && !"java.lang.Object".equals(superclass.toString())) {
                    stSb.append("extends ").append(simpleTypeName(superclass.toString()));
                }

                List<? extends TypeMirror> ifaces = te.getInterfaces();
                if (ifaces != null && !ifaces.isEmpty()) {
                    if (stSb.length() > 0) {
                        stSb.append(" ");
                    }
                    stSb.append("implements ").append(
                            ifaces.stream()
                                    .map(i -> simpleTypeName(i.toString()))
                                    .collect(Collectors.joining(", "))
                    );
                }

                if (stSb.length() > 0) {
                    comp.setSupertypes(stSb.toString());
                }
            } catch (Throwable t) {
                log.warn("Failed to resolve supertypes for {}: {}", comp.getFqn(), t.getMessage(), t);
                comp.setSupertypes("⚠️ [Supertypes unavailable]");
                scanWarnings.add("`" + comp.getSimpleName() + "`: Failed to resolve supertypes: " + t.getMessage());
            }
        }

        if (scope.isShowJavadoc()) {
            try {
                String doc = elements.getDocComment(te);
                if (doc != null && !doc.isBlank()) {
                    comp.setJavadocSummary(extractFirstSentence(doc));
                }
            } catch (Throwable t) {
                log.warn("Failed to resolve Javadoc for {}: {}", comp.getFqn(), t.getMessage(), t);
                comp.setJavadocSummary("⚠️ [Javadoc unavailable]");
                scanWarnings.add("`" + comp.getSimpleName() + "`: Failed to resolve Javadoc: " + t.getMessage());
            }
        }
    }

    /**
     * Recursively walks the directory structure to identify Java packages and their contents directly from disk.
     *
     * @param root The source root folder.
     * @param current The current folder being walked.
     * @param pkgMap The package accumulator map.
     * @param javaComponents The list to collect primary Java source components for metadata enrichment.
     * @param scope The active project structure granularity scope.
     * @throws Exception if filesystem operations fail.
     */
    private void walkJavaPackages(FileObject root, FileObject current, Map<String, JavaPackage> pkgMap, List<ProjectComponent> javaComponents, ProjectStructureScope scope) throws Exception {
        String relPkgPath = FileUtil.getRelativePath(root, current);
        String pkgName = (relPkgPath == null || relPkgPath.isEmpty()) ? "" : relPkgPath.replace('/', '.');
        
        JavaPackage pkg = pkgMap.computeIfAbsent(pkgName, k -> JavaPackage.builder().name(k).build());

        for (FileObject child : current.getChildren()) {
            if (child.isFolder()) {
                walkJavaPackages(root, child, pkgMap, javaComponents, scope);
            } else {
                String ext = child.getExt();
                if ("java".equals(ext)) {
                    String simpleName = child.getName();
                    if ("package-info".equals(simpleName)) {
                        ProjectComponent comp = new ProjectComponent(child, null);
                        pkg.addComponent(comp);
                        if (scope != null && scope.isShowJavadoc()) {
                            extractPackageInfoJavadoc(child, pkg);
                        }
                    } else {
                        String fqn = pkgName.isEmpty() ? simpleName : pkgName + "." + simpleName;
                        ProjectComponent comp = new ProjectComponent(child, null);
                        comp.setFqn(fqn);
                        pkg.addComponent(comp);
                        javaComponents.add(comp);
                    }
                } else {
                    ProjectComponent comp = new ProjectComponent(child, null);
                    pkg.addComponent(comp);
                }
            }
        }
        
        // Clean up empty packages
        if (pkg.getComponents().isEmpty() && pkgMap.containsKey(pkgName)) {
            pkgMap.remove(pkgName);
        }
    }

    /**
     * Extracts package-level Javadoc summary from package-info.java (supporting traditional Javadoc and JEP 467 /// comments).
     *
     * @param file The package-info.java FileObject.
     * @param pkg The target JavaPackage to enrich.
     */
    private void extractPackageInfoJavadoc(FileObject file, JavaPackage pkg) {
        try {
            String text = file.asText();
            String comment = null;
            int start = text.indexOf("/**");
            int end = text.indexOf("*/", start);
            if (start != -1 && end != -1) {
                comment = text.substring(start + 3, end);
            } else if (text.contains("///")) {
                StringBuilder sb = new StringBuilder();
                for (String line : text.split("\\R")) {
                    String trimmed = line.trim();
                    if (trimmed.startsWith("///")) {
                        sb.append(trimmed.substring(3).trim()).append(" ");
                    } else if (!trimmed.isEmpty() && !trimmed.startsWith("//")) {
                        break;
                    }
                }
                comment = sb.toString();
            }
            if (comment != null && !comment.isBlank()) {
                pkg.setJavadocSummary(extractFirstSentence(comment));
            }
        } catch (Exception e) {
            log.debug("Failed to read package-info Javadoc for {}: {}", file.getPath(), e.getMessage());
        }
    }

    /**
     * Strips package prefixes from type strings, preserving simple generic parameters.
     * E.g. {@code "java.util.List<java.lang.String>"} becomes {@code "List<String>"}.
     *
     * @param fqn The type name to simplify.
     * @return The simplified type name.
     */
    public static String simpleTypeName(String fqn) {
        if (fqn == null || fqn.isBlank()) {
            return "";
        }
        return fqn.replaceAll("([a-zA-Z_][a-zA-Z0-9_]*\\.)+", "");
    }

    /**
     * Extracts the first sentence from a Javadoc comment string by delegating to the unified core helper.
     *
     * @param docComment The raw Javadoc comment text.
     * @return The sanitized first sentence.
     */
    public static String extractFirstSentence(String docComment) {
        return AbstractProjectContextProvider.extractFirstSentence(docComment);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Calculates the total recursive size of all logical packages
     * contained within this group.
     * </p>
     */
    @Override
    public long getTotalSize() {
        return packages.stream().mapToLong(JavaPackage::getTotalSize).sum();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * 1. Renders the source group header (display name and relative path).
     * 2. Recursively triggers rendering for all constituent packages.
     * </p>
     */
    @Override
    public void renderMarkdown(StringBuilder sb, String indent, ProjectStructureScope scope) {
        sb.append("\n").append(indent).append("### ").append(name);
        if (relPath != null && !relPath.isEmpty()) {
            sb.append(" (`").append(relPath).append("`) ");
        }
        sb.append("\n");

        if (packages.isEmpty()) {
            sb.append(indent).append("  - (Empty)\n");
            return;
        }

        packages.sort(Comparator.comparing(JavaPackage::getName));

        for (JavaPackage pkg : packages) {
            pkg.renderMarkdown(sb, indent, scope);
        }
    }
}
