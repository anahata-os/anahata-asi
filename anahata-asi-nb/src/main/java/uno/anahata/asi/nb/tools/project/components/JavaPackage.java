/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.components;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;
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
public final class JavaPackage extends ProjectNode {

    /** 
     * The fully qualified name of the Java package. 
     */
    private String name;

    /** 
     * The list of top-level Java components (types) contained within this package. 
     */
    @Builder.Default
    private List<ProjectComponent> components = new ArrayList<>();
    
    /**
     * Adds a top-level Java component to this package.
     * 
     * @param component The component to add.
     */
    public void addComponent(ProjectComponent component) {
        this.components.add(component);
    }

    /** 
     * Calculates the total recursive size of all components in this package.
     * <p>
     * Implementation details:
     * Iterates through all registered components and sums their results 
     * from {@link ProjectComponent#getTotalSize()}.
     * </p>
     * 
     * @return The total byte count for the package.
     */
    @Override
    public long getTotalSize() {
        return components.stream().mapToLong(ProjectComponent::getTotalSize).sum();
    }

    /** 
     * Renders the package and its contents into a Markdown representation.
     * <p>
     * Implementation details:
     * 1. Renders the package name prefixed with the 📦 icon.
     * 2. In 'summary' mode, appends aggregate totals by component type (e.g. 2 CLASS, 1 png) and size.
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

        sb.append(indent).append("- 📦 `").append(name).append("` ");
        
        if (summary) {
            Map<String, Long> counts = components.stream()
                    .collect(Collectors.groupingBy(ProjectComponent::getComponentType, Collectors.counting()));
            
            String stats = counts.entrySet().stream()
                    .map(e -> e.getValue() + " " + e.getKey())
                    .collect(Collectors.joining(", "));
            
            sb.append("(").append(stats).append(") [").append(TextUtils.formatSize(totalSize)).append("]");
        }
        
        sb.append("\n");

        if (!summary) {
            List<ProjectComponent> sorted = new ArrayList<>(components);
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

            for (ProjectComponent component : sorted) {
                component.renderMarkdown(sb, indent + "  ", false);
            }
        }
    }
}
