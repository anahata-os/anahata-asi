/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.awt.Component;
import javax.swing.Icon;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.icons.TableIcon;
import uno.anahata.asi.swing.icons.ToolIcon;

/**
 * A custom TreeCellRenderer for the context tree that provides specific icons 
 * and labels for different types of context nodes.
 * <p>
 * It prioritizes specialized icons provided by the nodes themselves (e.g., 
 * NetBeans project or file icons) and ensures that the node's display name 
 * is used instead of its {@code toString()} representation.
 * </p>
 * 
 * @author anahata
 */
public class ContextTreeCellRenderer extends DefaultTreeCellRenderer {

    /** Default icon for context providers. */
    private final Icon providerIcon = IconUtils.getIcon("javadoc.png", 16);
    /** Default icon for toolkits. */
    private final Icon toolkitIcon = IconUtils.getIcon("java.png", 16);
    /** Programmatic icon for individual tools. */
    private final Icon toolIcon = new ToolIcon(16);
    /** Default icon for resources and the resources root node. */
    private final Icon resourceIcon = new TableIcon(16);
    /** Default icon for the history root node. */
    private final Icon historyIcon = IconUtils.getIcon("auto_prune.png", 16);
    /** Default icon for individual messages. */
    private final Icon messageIcon = IconUtils.getIcon("email.png", 16); 
    /** Default icon for individual message parts. */
    private final Icon partIcon = IconUtils.getIcon("copy.png", 16);

    /**
     * {@inheritDoc}
     * Implementation details: 
     * 1. Sets the text to the node's name.
     * 2. Inspects the node for a specialized icon, otherwise falls back to 
     *    a default icon based on the node type.
     */
    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
        super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);

        if (value instanceof AbstractContextNode<?> node) {
            setText(node.getName());
            
            Icon specializedIcon = node.getIcon();
            if (specializedIcon != null) {
                setIcon(specializedIcon);
            } else {
                // Fallback to default icons
                if (node instanceof ProviderNode) {
                    setIcon(providerIcon);
                } else if (node instanceof ToolkitNode) {
                    setIcon(toolkitIcon);
                } else if (node instanceof ToolNode) {
                    setIcon(toolIcon);
                } else if (node instanceof ResourcesNode || node instanceof ResourceNode) {
                    setIcon(resourceIcon);
                } else if (node instanceof HistoryNode) {
                    setIcon(historyIcon);
                } else if (node instanceof MessageNode) {
                    setIcon(messageIcon);
                } else if (node instanceof PartNode) {
                    setIcon(partIcon);
                }
            }
        }

        return this;
    }
}
