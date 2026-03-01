/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.components;

import java.io.Serializable;

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
     * Renders the node and its children into a Markdown representation.
     * <p>
     * Implementation details:
     * The renderer must strictly honor the 'summary' flag. In summary mode, 
     * it should only output aggregate totals for the node. In standard mode, 
     * it must perform a hierarchical traversal, using indentation to represent 
     * nesting depth and specialized icons to denote node types.
     * </p>
     * 
     * @param sb The target StringBuilder to append the Markdown to.
     * @param indent The current indentation string (e.g., "  ") for nesting level.
     * @param summary If true, renders only the condensed/aggregate view.
     */
    public abstract void renderMarkdown(StringBuilder sb, String indent, boolean summary);
}
