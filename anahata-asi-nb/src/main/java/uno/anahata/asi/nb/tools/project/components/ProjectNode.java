/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.components;

import java.io.Serializable;
import uno.anahata.asi.toolkit.project.ProjectStructureScope;

/**
 * The abstract base class for all structural nodes in the project model.
 * It defines the foundational contract for recursive metadata aggregation 
 * and Markdown-based visualization.
 * 
 * @author Anahata
 */
public abstract class ProjectNode implements Serializable {

    /**
     * Calculates the total recursive size of this node and all its descendants.
     * <p>
     * Implementation details:
     * This method must perform a deep traversal of the subtree rooted at this 
     * node, summing the sizes of all constituent physical files or logical 
     * components to provide an accurate total byte count for the branch.
     * </p>
     * 
     * @return The total size in bytes.
     */
    public abstract long getTotalSize();

    /**
     * Renders the node and its children into a Markdown representation respecting the granularity scope.
     *
     * @param sb The target StringBuilder to append the Markdown to.
     * @param indent The current indentation string (e.g., "  ") for nesting level.
     * @param scope The active project structure granularity scope.
     */
    public abstract void renderMarkdown(StringBuilder sb, String indent, ProjectStructureScope scope);
}
