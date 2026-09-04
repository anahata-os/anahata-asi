/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.components;

import java.awt.Component;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import javax.swing.AbstractCellEditor;
import javax.swing.Icon;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.table.TableCellEditor;
import lombok.Getter;
import lombok.NonNull;

/**
 * A generic, reusable {@link TableCellEditor} for editing sets or lists of {@link Enum} constants.
 * <p>
 * Employs {@link EnumSetComboBox} as the interactive editing component, providing a sleek dropdown
 * with real-time multi-checkbox toggling and automatic commit upon popup closure.
 * </p>
 *
 * @param <E> The specific enum type.
 * @author anahata
 */
public class EnumSetTableCellEditor<E extends Enum<E>> extends AbstractCellEditor implements TableCellEditor {

    /** The embedded multi-selection combo component. */
    @Getter
    private final EnumSetComboBox<E> comboBox;

    /**
     * Constructs a new EnumSetTableCellEditor.
     *
     * @param enumClass The target enum class.
     * @param iconProvider Optional function mapping enum constants to icons.
     * @param labelProvider Optional function mapping enum constants to labels.
     */
    public EnumSetTableCellEditor(
            @NonNull Class<E> enumClass,
            Function<E, Icon> iconProvider,
            Function<E, String> labelProvider
    ) {
        this.comboBox = new EnumSetComboBox<>(enumClass, null, iconProvider, labelProvider, null);
        this.comboBox.addPopupMenuListener(new PopupMenuListener() {
            @Override
            public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
            }

            @Override
            public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
                SwingUtilities.invokeLater(() -> stopCellEditing());
            }

            @Override
            public void popupMenuCanceled(PopupMenuEvent e) {
                SwingUtilities.invokeLater(() -> cancelCellEditing());
            }
        });
    }

    /**
     * {@inheritDoc}
     * <p>
     * Populates the combo component with the cell's current enum collection and returns the combo editor.
     * </p>
     */
    @Override
    public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
        Set<E> currentSet = new HashSet<>();
        if (value instanceof Collection<?> col) {
            for (Object obj : col) {
                if (obj != null) {
                    try {
                        @SuppressWarnings("unchecked")
                        E item = (E) obj;
                        currentSet.add(item);
                    } catch (ClassCastException ignored) {
                    }
                }
            }
        }
        comboBox.setSelectedValues(currentSet);
        return comboBox;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the selected enum constants as an ordered {@link List}.
     * </p>
     */
    @Override
    public Object getCellEditorValue() {
        List<E> result = new ArrayList<>(comboBox.getSelectedValues());
        result.sort(Enum::compareTo);
        return result;
    }
}
