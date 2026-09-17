/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.util.Locale;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.tree.TreePath;
import lombok.NonNull;
import org.jdesktop.swingx.JXTreeTable;
import uno.anahata.asi.swing.agi.AgiPanel;
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

    /** The parent AgiPanel used to resolve instance-based SwingAgiConfig. */
    private final AgiPanel agiPanel;

    /**
     * Constructs a {@code ContextTableCellRenderer} bound to an AgiPanel with default left alignment.
     *
     * @param agiPanel The parent AgiPanel.
     */
    public ContextTableCellRenderer(@NonNull AgiPanel agiPanel) {
        this(agiPanel, SwingConstants.LEFT);
    }

    /**
     * Constructs a {@code ContextTableCellRenderer} bound to an AgiPanel with a specific horizontal alignment.
     *
     * @param agiPanel The parent AgiPanel.
     * @param horizontalAlignment One of the alignment constants defined in {@link SwingConstants}.
     */
    public ContextTableCellRenderer(@NonNull AgiPanel agiPanel, int horizontalAlignment) {
        super();
        this.agiPanel = agiPanel;
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
        c.setFont(table.getFont()); // Always reset font to table default to prevent bold leakage

        if (table instanceof JXTreeTable treeTable) {
            TreePath path = treeTable.getPathForRow(row);
            if (path != null && path.getLastPathComponent() instanceof ResourceNode rn) {
                if (rn.isTruncated() && c instanceof JLabel label) {
                    label.setToolTipText(String.format(Locale.US, "Partial View: %.1f%% visible (use setFullView to expand)", rn.getVisiblePercentage()));
                }
            }
        }

        if (!isSelected) {
            c.setForeground(table.getForeground());
            c.setBackground(table.getBackground());
        } else {
            c.setForeground(table.getSelectionForeground());
            c.setBackground(table.getSelectionBackground());
        }
        setToolTipText(null);
        return c;
    }
}
