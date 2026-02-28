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
 * types into a hierarchical package-centric view. It distinguishes between 
 * top-level types and inner classes to provide an architecturally accurate map.
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
     * Constructs and populates the Java source group logic.
     * <p>
     * Implementation details:
     * 1. Uses the NetBeans ClassIndex to find all primary types within the root.
     * 2. Resolves each type's physical FileObject to capture size and status.
     * 3. Groups components into their respective Java packages.
     * 4. Post-processes the component map to establish nesting relationships 
     *    for inner and nested classes.
     * </p>
     * 
     * @param project The parent project.
     * @param sg The NetBeans source group instance.
     * @throws Exception if index access or file resolution fails.
     */
    public JavaSourceGroup2(Project project, SourceGroup sg) throws Exception {
        this.name = sg.getDisplayName();
        this.relPath = FileUtil.getRelativePath(project.getProjectDirectory(), sg.getRootFolder());
        this.packages = new ArrayList<>();

        Map<String, JavaPackage2> pkgMap = new TreeMap<>();
        ClasspathInfo cpInfo = ClasspathInfo.create(sg.getRootFolder());
        ClassIndex index = cpInfo.getClassIndex();
        Set<ElementHandle<javax.lang.model.element.TypeElement>> allTypes = index.getDeclaredTypes("", ClassIndex.NameKind.PREFIX, EnumSet.of(ClassIndex.SearchScope.SOURCE));
        
        Map<String, ProjectComponent2> typeMap = new HashMap<>();

        for (ElementHandle<javax.lang.model.element.TypeElement> handle : allTypes) {
            FileObject fo = SourceUtils.getFile(handle, cpInfo);
            if (fo == null) {
                continue;
            }

            ProjectComponent2 comp = new ProjectComponent2(fo, handle);
            typeMap.put(comp.getFqn(), comp);

            String pkgName = getPackageName(handle.getQualifiedName());
            pkgMap.computeIfAbsent(pkgName, k -> JavaPackage2.builder().name(k).build())
                  .addComponent(comp);
        }
        
        // Establish nesting relationships
        for (ProjectComponent2 comp : new ArrayList<>(typeMap.values())) {
            String fqn = comp.getFqn();
            int lastDot = fqn.lastIndexOf('.');
            if (lastDot != -1) {
                String parentFqn = fqn.substring(0, lastDot);
                if (typeMap.containsKey(parentFqn)) {
                    typeMap.get(parentFqn).addChild(comp);
                    String pkgName = getPackageName(fqn);
                    pkgMap.get(pkgName).getComponents().remove(comp);
                }
            }
        }
        this.packages.addAll(pkgMap.values());
    }

    /**
     * Extracts the logical package name from a fully qualified type name.
     * <p>
     * Uses a simple heuristic: the package name consists of all parts 
     * before the first part that starts with an uppercase letter.
     * </p>
     * 
     * @param fqn The fully qualified name.
     * @return The package name string.
     */
    private String getPackageName(String fqn) {
        int lastDot = fqn.lastIndexOf('.');
        if (lastDot == -1) {
            return "";
        }
        String[] parts = fqn.split("\\.");
        StringBuilder pkg = new StringBuilder();
        for (int i = 0; i < parts.length; i++) {
            if (Character.isUpperCase(parts[i].charAt(0))) {
                break;
            }
            if (i > 0) {
                pkg.append(".");
            }
            pkg.append(parts[i]);
        }
        return pkg.toString();
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
     * <p>
     * Implementation details:
     * Outputs a level-3 header for the group name and relative path, then 
     * sorts and renders each logical package.
     * </p>
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
