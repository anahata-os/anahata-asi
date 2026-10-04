/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.components;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import uno.anahata.asi.internal.TextUtils;
import uno.anahata.asi.ide.tools.project.ProjectStructureScope;

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
public final class ResourceFolder extends ProjectNode {

    /** 
     * The relative path of the folder from the source group root. 
     */
    private String path;

    /** 
     * The list of components (files) contained within this folder. 
     */
    @Builder.Default
    private List<ProjectComponent> components = new ArrayList<>();
    
    /**
     * Adds a physical component (file) to this folder.
     * 
     * @param component The component to add.
     */
    public void addComponent(ProjectComponent component) {
        this.components.add(component);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Calculates the total recursive size of all files in this folder.
     * </p>
     */
    @Override
    public long getTotalSize() {
        return components.stream().mapToLong(ProjectComponent::getTotalSize).sum();
    }

    /**
     * {@inheritDoc}
     * <p>
     * 1. Renders the folder path prefixed with the 📂 icon.
     * 2. In 'summary' mode, appends aggregate totals by file extension (e.g. 1 png, 3 md) 
     *    and the total recursive size.
     * 3. In standard mode, recursively triggers rendering for all child components.
     * </p>
     */
    @Override
    public void renderMarkdown(StringBuilder sb, String indent, ProjectStructureScope scope) {
        sb.append(indent).append("- 📂 `").append(path).append("`\n");
        for (ProjectComponent component : components) {
            component.renderMarkdown(sb, indent + "  ", scope);
        }
    }
}
