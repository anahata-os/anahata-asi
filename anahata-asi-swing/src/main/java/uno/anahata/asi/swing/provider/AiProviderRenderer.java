/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.provider;

import java.awt.Component;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * A unified, reusable Swing cell renderer for {@link AbstractAiProvider} instances.
 * <p>
 * This renderer implements both {@link javax.swing.ListCellRenderer} (for dropdowns like {@link javax.swing.JComboBox}
 * and {@link javax.swing.JList}) and {@link javax.swing.table.TableCellRenderer} (for {@link javax.swing.JTable}
 * and {@link org.jdesktop.swingx.JXTable}). It renders the provider's official logo icon alongside its display name
 * or UUID, and formats null values cleanly as "All AI Providers".
 * </p>
 * 
 * @author anahata
 */
public class AiProviderRenderer extends DefaultListCellRenderer implements TableCellRenderer {

    /**
     * Delegate renderer used for table cell background, selection, and border styling.
     */
    private final DefaultTableCellRenderer tableRenderer = new DefaultTableCellRenderer();

    /**
     * {@inheritDoc}
     * <p>
     * Configures the list/combobox cell renderer component with the provider's logo icon and display name.
     * </p>
     *
     * @param list The JList being rendered.
     * @param value The value to assign to the cell (typically an {@link AbstractAiProvider} or {@code null}).
     * @param index The cell index.
     * @param isSelected True if the cell is selected.
     * @param cellHasFocus True if the cell has focus.
     * @return The configured list cell component.
     */
    @Override
    public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
        super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        configure(this, value);
        return this;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Configures the table cell renderer component with the provider's logo icon and display name.
     * </p>
     *
     * @param table The JTable being rendered.
     * @param value The cell value (typically an {@link AbstractAiProvider} or {@code null}).
     * @param isSelected True if the cell is selected.
     * @param hasFocus True if the cell has focus.
     * @param row The row index of the cell.
     * @param column The column index of the cell.
     * @return The configured table cell component.
     */
    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
        Component comp = tableRenderer.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
        configure(comp, value);
        return comp;
    }

    /**
     * Configures a target label component with provider iconography and label text.
     *
     * @param comp The target component to configure.
     * @param value The provider entity or placeholder object.
     */
    private static void configure(Component comp, Object value) {
        if (comp instanceof JLabel label) {
            if (value instanceof AbstractAiProvider p) {
                label.setText(p.getDisplayName() != null ? p.getDisplayName() : p.getUuid());
                Icon icon = IconUtils.getIcon("aiproviders/" + p.getClass().getName() + ".png", 16, 16);
                label.setIcon(icon);
            } else if (value == null) {
                label.setText("All AI Providers");
                label.setIcon(null);
            } else {
                label.setText(value.toString());
                label.setIcon(null);
            }
        }
    }
}
