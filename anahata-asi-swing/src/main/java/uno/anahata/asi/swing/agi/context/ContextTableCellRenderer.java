/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context;

import java.awt.Color;
import java.awt.Component;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.tree.TreePath;
import org.jdesktop.swingx.JXTreeTable;
import uno.anahata.asi.swing.agi.SwingAgiConfig;
import uno.anahata.asi.swing.agi.resources.ResourceNode;

/**
 * Custom table cell renderer for token metrics and status columns in the Context TreeTable.
 * <p>
 * This renderer detects whether a rendered node is a {@link ResourceNode} whose active
 * viewport is truncated (i.e. only a partial slice of the resource file is loaded into the
 * AI prompt window). When truncated, token metrics are highlighted in a theme-adaptive
 * warning/orange color provided by {@link SwingAgiConfig#getTruncatedTokenColor()}.
 * </p>
 * <p>
 * <b>Look and Feel Adaptability:</b> Supports both Dark and Light Swing themes dynamically,
 * ensuring high contrast and readability on all platforms and desktop environments.
 * </p>
 *
 * @author anahata
 */
public class ContextTableCellRenderer extends DefaultTableCellRenderer {

    /**
     * Constructs a default {@code ContextTableCellRenderer} with standard left alignment.
     */
    public ContextTableCellRenderer() {
        super();
    }

    /**
     * Constructs a {@code ContextTableCellRenderer} with a specific horizontal text alignment.
     *
     * @param horizontalAlignment One of the alignment constants defined in {@link javax.swing.SwingConstants}
     *                            (e.g., {@code javax.swing.SwingConstants.RIGHT} or {@code javax.swing.SwingConstants.LEFT}).
     */
    public ContextTableCellRenderer(int horizontalAlignment) {
        super();
        setHorizontalAlignment(horizontalAlignment);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Inspects the target row in the parent {@link JXTreeTable} to determine
     * if the node corresponds to a truncated {@link ResourceNode}. If truncated, the foreground text
     * color is painted with an adaptive warning/orange color and an explanatory tooltip is attached.
     * </p>
     *
     * @param table The {@code JTable} requesting rendering.
     * @param value The value of the cell to be rendered.
     * @param isSelected True if the cell is currently selected in the UI.
     * @param hasFocus True if the cell currently holds keyboard focus.
     * @param row The row index of the cell being painted.
     * @param column The column index of the cell being painted.
     * @return The configured component for rendering.
     */
    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
        Component c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

        if (table instanceof JXTreeTable treeTable) {
            TreePath path = treeTable.getPathForRow(row);
            if (path != null && path.getLastPathComponent() instanceof ResourceNode rn) {
                if (rn.isTruncated()) {
                    Color warningColor = SwingAgiConfig.getTruncatedTokenColor();
                    if (!isSelected) {
                        c.setForeground(warningColor);
                    } else {
                        c.setForeground(SwingAgiConfig.isDarkLaf() ? new Color(255, 210, 120) : new Color(255, 235, 190));
                    }
                    if (c instanceof JLabel label) {
                        label.setToolTipText("Viewport is truncated (only partial file content loaded into prompt)");
                    }
                    return c;
                }
            }
        }

        if (!isSelected) {
            c.setForeground(table.getForeground());
        } else {
            c.setForeground(table.getSelectionForeground());
        }
        setToolTipText(null);
        return c;
    }
}
