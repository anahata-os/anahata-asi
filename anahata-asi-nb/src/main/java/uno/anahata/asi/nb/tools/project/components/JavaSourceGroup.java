/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.components;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
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
import org.netbeans.api.java.source.ClassIndex;
import org.netbeans.api.java.source.ClasspathInfo;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.java.source.JavaSource.Phase;
import org.netbeans.api.java.source.SourceUtils;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.SourceGroup;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.toolkit.project.AbstractProjectContextProvider;
import uno.anahata.asi.toolkit.project.ProjectStructureScope;

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
     * Constructs and populates the Java source group logic using a hybrid approach.
     * <p>
     * Implementation details:
     * 1. Queries the NetBeans ClassIndex for all declared types in the group.
     * 2. Resolves these types to their physical FileObjects.
     * 3. Performs a recursive physical walk of the source root.
     * 4. Merges the results: for each file, it either adds the logical types 
     *    discovered from the index or adds the file itself if no types were indexed 
     *    (handles package-info.java and non-java files).
     * 5. Post-processes the component map to establish nesting relationships.
     * </p>
     * 
     * @param project The parent project.
     * @param sg The NetBeans source group instance.
     * @throws Exception if index access or physical walk fails.
     */
    public JavaSourceGroup(Project project, SourceGroup sg) throws Exception {
        this(project, sg, new ProjectStructureScope(), new ArrayList<>());
    }

    public JavaSourceGroup(Project project, SourceGroup sg, ProjectStructureScope scope) throws Exception {
        this(project, sg, scope, new ArrayList<>());
    }

    public JavaSourceGroup(Project project, SourceGroup sg, ProjectStructureScope scope, List<String> scanWarnings) throws Exception {
        this.name = sg.getDisplayName();
        this.relPath = FileUtil.getRelativePath(project.getProjectDirectory(), sg.getRootFolder());
        this.packages = new ArrayList<>();

        FileObject root = sg.getRootFolder();
        ClasspathInfo cpInfo = ClasspathInfo.create(root);
        ClassIndex index = cpInfo.getClassIndex();
        Set<ElementHandle<TypeElement>> allTypes = index.getDeclaredTypes("", ClassIndex.NameKind.PREFIX, EnumSet.of(ClassIndex.SearchScope.SOURCE));
        
        Map<FileObject, List<ProjectComponent>> fileToComponents = new HashMap<>();
        Map<String, ProjectComponent> fqnToComponent = new HashMap<>();

        for (ElementHandle<TypeElement> handle : allTypes) {
            FileObject fo = SourceUtils.getFile(handle, cpInfo);
            if (fo == null) {
                continue;
            }

            ProjectComponent comp = new ProjectComponent(fo, handle);
            fqnToComponent.put(comp.getFqn(), comp);
            fileToComponents.computeIfAbsent(fo, k -> new ArrayList<>()).add(comp);
        }
        
        // Establish nesting relationships for indexed types
        for (ProjectComponent comp : new ArrayList<>(fqnToComponent.values())) {
            String fqn = comp.getFqn();
            int lastDot = fqn.lastIndexOf('.');
            if (lastDot != -1) {
                String parentFqn = fqn.substring(0, lastDot);
                if (fqnToComponent.containsKey(parentFqn)) {
                    ProjectComponent parentComp = fqnToComponent.get(parentFqn);
                    parentComp.addChild(comp);
                    // Remove from the file's primary components list as it is now a child
                    fileToComponents.get(comp.getFileObject()).remove(comp);
                }
            }
        }

        if (scope != null && (scope.isShowSupertypes() || scope.isShowJavadoc())) {
            resolveAstMetadata(cpInfo, fqnToComponent.values(), scope, scanWarnings != null ? scanWarnings : new ArrayList<>());
        }

        // Perform physical walk to find all files and group them into packages
        Map<String, JavaPackage> pkgMap = new TreeMap<>();
        walkJavaPackages(root, root, fileToComponents, pkgMap, scope);
        this.packages.addAll(pkgMap.values());
    }

    /**
     * Resolves supertypes and class-level Javadoc comments in a fast, single-pass JavaSource task.
     * <p>
     * Implementation details:
     * 1. Uses {@link JavaSource#create(ClasspathInfo)} to initialize a single shared compilation context.
     * 2. Advances to {@link Phase#ELEMENTS_RESOLVED} once for all types, avoiding redundant per-file javac parsing.
     * 3. Resolves each type element and extracts superclass, implemented interfaces, and doc comments.
     * 4. For any handles that fail batch resolution, falls back to per-file resolution.
     * </p>
     *
     * @param cpInfo The classpath info for the source root.
     * @param components The collection of project components to enrich.
     * @param scope The active project structure granularity scope.
     * @param scanWarnings List to collect any resolution warnings.
     */
    private void resolveAstMetadata(ClasspathInfo cpInfo, Collection<ProjectComponent> components, ProjectStructureScope scope, List<String> scanWarnings) {
        List<ProjectComponent> unresolved = new ArrayList<>();
        try {
            JavaSource js = JavaSource.create(cpInfo);
            if (js != null) {
                js.runUserActionTask(controller -> {
                    controller.toPhase(Phase.ELEMENTS_RESOLVED);
                    Elements elements = controller.getElements();

                    for (ProjectComponent comp : components) {
                        @SuppressWarnings("unchecked")
                        ElementHandle<TypeElement> handle = (ElementHandle<TypeElement>) comp.getHandle();
                        if (handle != null) {
                            TypeElement te = handle.resolve(controller);
                            if (te != null) {
                                populateTypeMetadata(comp, te, elements, scope, scanWarnings);
                            } else {
                                unresolved.add(comp);
                            }
                        }
                    }
                }, true);
            }
        } catch (Throwable e) {
            log.warn("Batch AST metadata resolution failed, falling back to per-file: {}", e.getMessage(), e);
            unresolved.addAll(components);
        }

        for (ProjectComponent comp : unresolved) {
            FileObject fo = comp.getFileObject();
            if (fo != null && fo.isValid()) {
                try {
                    JavaSource fileJs = JavaSource.forFileObject(fo);
                    if (fileJs != null) {
                        fileJs.runUserActionTask(controller -> {
                            controller.toPhase(Phase.ELEMENTS_RESOLVED);
                            @SuppressWarnings("unchecked")
                            ElementHandle<TypeElement> handle = (ElementHandle<TypeElement>) comp.getHandle();
                            if (handle != null) {
                                TypeElement te = handle.resolve(controller);
                                if (te != null) {
                                    populateTypeMetadata(comp, te, controller.getElements(), scope, scanWarnings);
                                }
                            }
                        }, true);
                    }
                } catch (Throwable t) {
                    log.warn("Per-file AST fallback failed for {}: {}", comp.getFqn(), t.getMessage(), t);
                }
            }
        }
    }

    /**
     * Extracts supertypes and class-level Javadoc comments from a resolved TypeElement.
     *
     * @param comp The project component to populate.
     * @param te The resolved TypeElement symbol.
     * @param elements The javac Elements utility.
     * @param scope The active granularity scope.
     * @param scanWarnings List to record any exceptions.
     */
    private void populateTypeMetadata(ProjectComponent comp, TypeElement te, Elements elements, ProjectStructureScope scope, List<String> scanWarnings) {
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
     * Recursively walks the directory structure to identify Java packages and their contents.
     *
     * @param root The source root folder.
     * @param current The current folder being walked.
     * @param fileToComponents The mapping of file objects to logical components.
     * @param pkgMap The package accumulator map.
     * @throws Exception if filesystem operations fail.
     */
    private void walkJavaPackages(FileObject root, FileObject current, Map<FileObject, List<ProjectComponent>> fileToComponents, Map<String, JavaPackage> pkgMap, ProjectStructureScope scope) throws Exception {
        String relPkgPath = FileUtil.getRelativePath(root, current);
        String pkgName = (relPkgPath == null || relPkgPath.isEmpty()) ? "" : relPkgPath.replace('/', '.');
        
        JavaPackage pkg = pkgMap.computeIfAbsent(pkgName, k -> JavaPackage.builder().name(k).build());

        for (FileObject child : current.getChildren()) {
            if (child.isFolder()) {
                walkJavaPackages(root, child, fileToComponents, pkgMap, scope);
            } else {
                List<ProjectComponent> indexed = fileToComponents.get(child);
                if (indexed != null && !indexed.isEmpty()) {
                    for (ProjectComponent comp : indexed) {
                        pkg.addComponent(comp);
                    }
                } else {
                    // Not indexed (package-info.java or resource)
                    ProjectComponent comp = new ProjectComponent(child, null);
                    pkg.addComponent(comp);

                    if (scope != null && scope.isShowJavadoc() && child.getNameExt().startsWith("package-info")) {
                        try {
                            String text = child.asText();
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
                            log.debug("Failed to read package-info Javadoc for {}: {}", child.getPath(), e.getMessage());
                        }
                    }
                }
            }
        }
        
        // Clean up empty packages
        if (pkg.getComponents().isEmpty() && pkgMap.containsKey(pkgName)) {
            pkgMap.remove(pkgName);
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
