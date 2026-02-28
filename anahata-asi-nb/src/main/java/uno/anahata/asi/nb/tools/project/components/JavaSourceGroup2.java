/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.components;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.netbeans.api.java.source.ClassIndex;
import org.netbeans.api.java.source.ClasspathInfo;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.api.java.source.SourceUtils;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.SourceGroup;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;

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
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public final class JavaSourceGroup2 extends ProjectNode2 {

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
    private List<JavaPackage2> packages = new ArrayList<>();

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
    public JavaSourceGroup2(Project project, SourceGroup sg) throws Exception {
        this.name = sg.getDisplayName();
        this.relPath = FileUtil.getRelativePath(project.getProjectDirectory(), sg.getRootFolder());
        this.packages = new ArrayList<>();

        FileObject root = sg.getRootFolder();
        ClasspathInfo cpInfo = ClasspathInfo.create(root);
        ClassIndex index = cpInfo.getClassIndex();
        Set<ElementHandle<javax.lang.model.element.TypeElement>> allTypes = index.getDeclaredTypes("", ClassIndex.NameKind.PREFIX, EnumSet.of(ClassIndex.SearchScope.SOURCE));
        
        Map<FileObject, List<ProjectComponent2>> fileToComponents = new HashMap<>();
        Map<String, ProjectComponent2> fqnToComponent = new HashMap<>();

        for (ElementHandle<javax.lang.model.element.TypeElement> handle : allTypes) {
            FileObject fo = SourceUtils.getFile(handle, cpInfo);
            if (fo == null) {
                continue;
            }

            ProjectComponent2 comp = new ProjectComponent2(fo, handle);
            fqnToComponent.put(comp.getFqn(), comp);
            fileToComponents.computeIfAbsent(fo, k -> new ArrayList<>()).add(comp);
        }
        
        // Establish nesting relationships for indexed types
        for (ProjectComponent2 comp : new ArrayList<>(fqnToComponent.values())) {
            String fqn = comp.getFqn();
            int lastDot = fqn.lastIndexOf('.');
            if (lastDot != -1) {
                String parentFqn = fqn.substring(0, lastDot);
                if (fqnToComponent.containsKey(parentFqn)) {
                    ProjectComponent2 parentComp = fqnToComponent.get(parentFqn);
                    parentComp.addChild(comp);
                    // Remove from the file's primary components list as it is now a child
                    fileToComponents.get(comp.getFileObject()).remove(comp);
                }
            }
        }

        // Perform physical walk to find all files and group them into packages
        Map<String, JavaPackage2> pkgMap = new TreeMap<>();
        walkJavaPackages(root, root, fileToComponents, pkgMap);
        this.packages.addAll(pkgMap.values());
    }

    /**
     * Recursively walks the directory structure to identify Java packages and their contents.
     */
    private void walkJavaPackages(FileObject root, FileObject current, Map<FileObject, List<ProjectComponent2>> fileToComponents, Map<String, JavaPackage2> pkgMap) throws Exception {
        String relPkgPath = FileUtil.getRelativePath(root, current);
        String pkgName = (relPkgPath == null || relPkgPath.isEmpty()) ? "" : relPkgPath.replace('/', '.');
        
        JavaPackage2 pkg = pkgMap.computeIfAbsent(pkgName, k -> JavaPackage2.builder().name(k).build());

        for (FileObject child : current.getChildren()) {
            if (child.isFolder()) {
                walkJavaPackages(root, child, fileToComponents, pkgMap);
            } else {
                List<ProjectComponent2> indexed = fileToComponents.get(child);
                if (indexed != null && !indexed.isEmpty()) {
                    for (ProjectComponent2 comp : indexed) {
                        pkg.addComponent(comp);
                    }
                } else {
                    // Not indexed (package-info.java or resource)
                    pkg.addComponent(new ProjectComponent2(child, null));
                }
            }
        }
        
        // Clean up empty packages
        if (pkg.getComponents().isEmpty() && pkgMap.containsKey(pkgName)) {
            pkgMap.remove(pkgName);
        }
    }

    /** 
     * Calculates the total recursive size of all packages in this group.
     * 
     * @return Total byte count.
     */
    @Override
    public long getTotalSize() {
        return packages.stream().mapToLong(JavaPackage2::getTotalSize).sum();
    }

    /** 
     * Renders the Java source group header and all constituent packages.
     * 
     * @param sb The target StringBuilder.
     * @param indent The current indentation level.
     * @param summary If true, renders condensed aggregate views.
     */
    @Override
    public void renderMarkdown(StringBuilder sb, String indent, boolean summary) {
        sb.append("\n").append(indent).append("### ").append(name);
        if (relPath != null && !relPath.isEmpty()) {
            sb.append(" (`").append(relPath).append("`) ");
        }
        sb.append("\n");

        if (packages.isEmpty()) {
            sb.append(indent).append("  - (Empty)\n");
            return;
        }

        packages.sort(Comparator.comparing(JavaPackage2::getName));

        for (JavaPackage2 pkg : packages) {
            pkg.renderMarkdown(sb, indent, summary);
        }
    }
}
