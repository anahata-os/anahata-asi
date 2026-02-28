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
 * A domain object representing a physical directory containing project resources.
 * <p>
 * This class groups non-Java files and handles the rendering of physical 
 * folder structures using the 📂 icon.
 * </p>
 * 
 * @author Anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public final class ResourceFolder2 extends ProjectNode2 {

    /** 
     * The relative path of the folder from the source group root. 
     */
    private String path;

    /** 
     * The list of components (files) contained within this folder. 
     */
    @Builder.Default
    private List<ProjectComponent2> components = new ArrayList<>();
    
    /**
     * Adds a physical component (file) to this folder.
     * 
     * @param component The component to add.
     */
    public void addComponent(ProjectComponent2 component) {
        this.components.add(component);
    }

    /** 
     * Calculates the total recursive size of all files in this folder.
     * 
     * @return Total byte count.
     */
    @Override
    public long getTotalSize() {
        return components.stream().mapToLong(ProjectComponent2::getTotalSize).sum();
    }

    /** 
     * Renders the folder and its files into a Markdown representation.
     * <p>
     * Implementation details:
     * 1. Renders the folder path prefixed with the 📂 icon.
     * 2. In 'summary' mode, appends aggregate file totals and size to the header.
     * 3. In standard mode, recursively triggers rendering for all child components.
     * </p>
     * 
     * @param sb The target StringBuilder.
     * @param indent The current indentation level.
     * @param summary If true, renders only the condensed aggregate view.
     */
    @Override
    public void renderMarkdown(StringBuilder sb, String indent, boolean summary) {
        long totalSize = getTotalSize();
        int fileCount = components.size();

        sb.append(indent).append("- 📂 `").append(path).append("` ");
        
        if (summary) {
            sb.append("(").append(fileCount).append(" files) [").append(TextUtils.formatSize(totalSize)).append("]");
        }
        
        sb.append("\n");

        if (!summary) {
            for (ProjectComponent2 component : components) {
                component.renderMarkdown(sb, indent + "  ", false);
            }
        }
    }
}
