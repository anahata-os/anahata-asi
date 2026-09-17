/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.util.Locale;
import javax.swing.Icon;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;
import lombok.NonNull;
import uno.anahata.asi.agi.tool.ToolManager;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.SwingAgiConfig;
import uno.anahata.asi.swing.agi.resources.ResourceNode;
import uno.anahata.asi.swing.icons.AtomsIcon;
import uno.anahata.asi.swing.icons.DoubleToolIconRefined;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.icons.ToolIcon;

/**
 * A custom TreeCellRenderer for the context tree that provides specific icons 
 * and labels for different types of context nodes.
 * <p>
 * It prioritizes specialized icons provided by the nodes themselves (e.g., 
 * NetBeans project or file icons) and ensures that the node's display name 
 * is used instead of its {@code toString()} representation.
 * </p>
 * <p>
 * It also implements automatic icon fading (desaturation) and text graying 
 * for inactive nodes (e.g., disabled toolkits or pruned messages).
 * </p>
 * 
 * @author anahata
 */
public class ContextTreeCellRenderer extends DefaultTreeCellRenderer {

    /** The parent AgiPanel used to resolve instance-based SwingAgiConfig. */
    private final AgiPanel agiPanel;

    /** Default icon for toolkits (authentic Java logo). */
    private final Icon toolkitIcon = IconUtils.getIcon("java.png", 16);
    /** Programmatic icon for individual tools. */
    private final Icon toolIcon = new ToolIcon(16);
    /** Refined programmatic icon for tool containers. */
    private final Icon toolsIcon = new DoubleToolIconRefined(16);
    /**
     * Specialized icon for the ToolManager node.
     */
    private final Icon toolManagerIcon = new AtomsIcon(16);
    /** Default icon for individual messages. */
    private final Icon messageIcon = IconUtils.getIcon("email.png", 16); 
    /** Default icon for individual message parts. */
    private final Icon partIcon = IconUtils.getIcon("copy.png", 16);

    /**
     * Constructs a ContextTreeCellRenderer bound to an AgiPanel.
     *
     * @param agiPanel The parent AgiPanel.
     */
    public ContextTreeCellRenderer(@NonNull AgiPanel agiPanel) {
        this.agiPanel = agiPanel;
    }

    /**
     * {@inheritDoc}
     * Implementation details: 
     * 1. Sets the text to the node's name.
     * 2. Inspects the node for a specialized icon, otherwise falls back to 
     *    a default icon based on the node type.
     * 3. If no specialized or type-specific icon is found, it preserves the 
     *    default JTree folder/file icons.
     * 4. Fades the icon and grays out the text if the node is inactive.
     */
    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
        Component c = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
        setFont(tree.getFont().deriveFont(Font.PLAIN)); // Reset to plain to kill bold leakage

        if (value instanceof AbstractContextNode<?> node) {
            setText(node.getName());

            Icon icon = node.getIcon();
            if (icon == null) {
                // Fallback to type-specific icons
                if (node instanceof ToolkitNode) {
                    icon = toolkitIcon;
                } else if (node instanceof ToolsNode) {
                    icon = toolsIcon;
                } else if (node instanceof ToolManagerNode) {
                    icon = toolManagerIcon;
                } else if (node instanceof ToolNode) {
                    icon = toolIcon;
                } else if (node instanceof MessageNode) {
                    icon = messageIcon;
                } else if (node instanceof PartNode) {
                    icon = partIcon;
                }
            }

            // If we found a specialized icon, apply it (potentially with fading)
            if (icon != null) {
                if (!node.isActive()) {
                    icon = IconUtils.getDisabledIcon(icon);
                }
                setIcon(icon);
            } else {
                // No specialized icon: preserve the default JTree icon set by super
                Icon defaultIcon = getIcon();
                if (defaultIcon != null && !node.isActive()) {
                    setIcon(IconUtils.getDisabledIcon(defaultIcon));
                }
            }

            // Gray out text for inactive nodes, or set bold & tooltip for truncated resources
            if (!node.isActive()) {
                setForeground(Color.GRAY);
            } else if (node instanceof ResourceNode rn && rn.isTruncated()) {
                setForeground(sel ? getTextSelectionColor() : getTextNonSelectionColor());
                setFont(getFont().deriveFont(Font.BOLD));
                setToolTipText(String.format(Locale.US, "Partial View: %.1f%% visible (use setFullView to expand)", rn.getVisiblePercentage()));
            } else {
                setForeground(sel ? getTextSelectionColor() : getTextNonSelectionColor());
            }
        }

        return this;
    }
}
