/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.provider;

import java.awt.Color;
import java.awt.Component;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JTable;
import javax.swing.UIManager;
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
        configure(this, value, isSelected);
        return this;
    }

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
        Component comp = tableRenderer.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
        configure(comp, value, isSelected);
        return comp;
    }

    /**
     * Configures a target label component with provider iconography, label text,
     * and visual graying out if the provider is disabled or missing required API keys.
     *
     * @param comp The target component to configure.
     * @param value The provider entity or placeholder object.
     * @param isSelected Whether the cell is currently selected.
     */
    private static void configure(Component comp, Object value, boolean isSelected) {
        if (comp instanceof JLabel label) {
            if (value instanceof AbstractAiProvider p) {
                label.setText(p.getDisplayName() != null ? p.getDisplayName() : p.getUuid());
                Icon icon = IconUtils.getIcon("aiproviders/" + p.getClass().getName() + ".png", 16, 16);
                boolean effectivelyEnabled = p.isEffectivelyEnabled();
                label.setEnabled(effectivelyEnabled);
                if (!effectivelyEnabled && icon != null) {
                    Icon disabledIcon = UIManager.getLookAndFeel().getDisabledIcon(label, icon);
                    label.setIcon(disabledIcon != null ? disabledIcon : icon);
                } else {
                    label.setIcon(icon);
                }
                if (!isSelected && !effectivelyEnabled) {
                    Color disabledFg = UIManager.getColor("Label.disabledForeground");
                    label.setForeground(disabledFg != null ? disabledFg : Color.GRAY);
                }
                if (!p.isEnabled()) {
                    label.setToolTipText("Provider is disabled");
                } else if (!p.isEffectivelyEnabled()) {
                    label.setToolTipText("Provider is enabled but has no configured API keys");
                } else {
                    label.setToolTipText(null);
                }
            } else if (value == null) {
                label.setText("All AI Providers");
                label.setIcon(null);
                label.setEnabled(true);
                label.setToolTipText(null);
            } else {
                label.setText(value.toString());
                label.setIcon(null);
                label.setEnabled(true);
                label.setToolTipText(null);
            }
        }
    }
}
