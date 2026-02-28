/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.components;

import java.util.ArrayList;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import uno.anahata.asi.internal.TextUtils;

/**
 * A domain object representing a logical Java package within a project.
 * <p>
 * This class handles the logical grouping of Java types and is responsible 
 * for rendering the package header with the 📦 icon and managing the 
 * summarized view of its contents.
 * </p>
 * 
 * @author Anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public final class JavaPackage2 extends ProjectNode2 {

    /** 
     * The fully qualified name of the Java package. 
     */
    private String name;

    /** 
     * The list of top-level Java components (types) contained within this package. 
     */
    @Builder.Default
    private List<ProjectComponent2> components = new ArrayList<>();
    
    /**
     * Adds a top-level Java component to this package.
     * 
     * @param component The component to add.
     */
    public void addComponent(ProjectComponent2 component) {
        this.components.add(component);
    }

    /** 
     * Calculates the total recursive size of all components in this package.
     * <p>
     * Implementation details:
     * Iterates through all registered components and sums their results 
     * from {@link ProjectComponent2#getTotalSize()}.
     * </p>
     * 
     * @return The total byte count for the package.
     */
    @Override
    public long getTotalSize() {
        return components.stream().mapToLong(ProjectComponent2::getTotalSize).sum();
    }

    /** 
     * Renders the package and its contents into a Markdown representation.
     * <p>
     * Implementation details:
     * 1. Renders the package name prefixed with the 📦 icon.
     * 2. In 'summary' mode, appends the file count and total size to the header.
     * 3. In standard mode, sorts components (ensuring package-info is first) 
     *    and recursively triggers their rendering logic.
     * </p>
     * 
     * @param sb The target StringBuilder.
     * @param indent The current indentation level.
     * @param summary If true, renders the condensed aggregate view.
     */
    @Override
    public void renderMarkdown(StringBuilder sb, String indent, boolean summary) {
        long totalSize = getTotalSize();
        int fileCount = components.size();

        sb.append(indent).append("- 📦 `").append(name).append("` ");
        
        if (summary) {
            sb.append("(").append(fileCount).append(" files) [").append(TextUtils.formatSize(totalSize)).append("]");
        }
        
        sb.append("\n");

        if (!summary) {
            List<ProjectComponent2> sorted = new ArrayList<>(components);
            sorted.sort((a, b) -> {
                String aName = a.getFileName() != null ? a.getFileName() : "";
                String bName = b.getFileName() != null ? b.getFileName() : "";
                if (aName.startsWith("package-info")) {
                    return -1;
                }
                if (bName.startsWith("package-info")) {
                    return 1;
                }
                
                String aVal = a.getFqn() != null ? a.getFqn() : aName;
                String bVal = b.getFqn() != null ? b.getFqn() : bName;
                return aVal.compareToIgnoreCase(bVal);
            });

            for (ProjectComponent2 component : sorted) {
                component.renderMarkdown(sb, indent + "  ", false);
            }
        }
    }
}
