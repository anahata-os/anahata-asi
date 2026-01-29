/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.awt.Component;
import javax.swing.Icon;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.icons.JavaIcon;
import uno.anahata.asi.swing.icons.LocalToolsIcon;
import uno.anahata.asi.swing.icons.TableIcon;

/**
 * A custom TreeCellRenderer for the context tree that provides specific icons 
 * for different types of context nodes.
 * <p>
 * It prioritizes specialized icons provided by the nodes themselves (e.g., 
 * NetBeans project or file icons) before falling back to default type-based icons.
 * </p>
 * 
 * @author anahata
 */
public class ContextTreeCellRenderer extends DefaultTreeCellRenderer {

    /** Default icon for context providers. */
    private final Icon providerIcon = new JavaIcon(16);
    /** Default icon for toolkits. */
    private final Icon toolkitIcon = new LocalToolsIcon(16);
    /** Default icon for individual tools. */
    private final Icon toolIcon = IconUtils.getIcon("run.png", 16, 16);
    /** Default icon for resources and the resources root node. */
    private final Icon resourceIcon = new TableIcon(16);
    /** Default icon for the history root node. */
    private final Icon historyIcon = IconUtils.getIcon("auto_prune.png", 16, 16);
    /** Default icon for individual messages. */
    private final Icon messageIcon = IconUtils.getIcon("microphone.png", 16, 16);
    /** Default icon for individual message parts. */
    private final Icon partIcon = IconUtils.getIcon("copy.png", 16, 16);

    /**
     * {@inheritDoc}
     * Implementation details: Inspects the node for a specialized icon, 
     * otherwise falls back to a default icon based on the node type.
     */
    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
        super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);

        if (value instanceof AbstractContextNode<?> node) {
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
