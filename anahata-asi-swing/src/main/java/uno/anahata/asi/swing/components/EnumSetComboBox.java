/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.components;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Insets;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.UIManager;
import lombok.Getter;
import lombok.NonNull;

/**
 * A sleek, generic Swing component for selecting multiple values from an {@link Enum} type.
 * <p>
 * Displays an interactive button showing active enum icons and summary text. When clicked,
 * it opens a persistent {@link JPopupMenu} with checkable rows for each enum constant,
 * allowing users to toggle multiple values in real-time.
 * </p>
 *
 * @param <E> The specific enum type.
 * @author anahata
 */
public class EnumSetComboBox<E extends Enum<E>> extends JButton {

    /** The target enum class. */
    private final Class<E> enumClass;
    /** The active set of selected enum constants. */
    @Getter
    private final Set<E> selectedValues = new HashSet<>();
    /** Optional function to provide icons for enum constants. */
    private final Function<E, Icon> iconProvider;
    /** Optional function to provide display labels for enum constants. */
    private final Function<E, String> labelProvider;
    /** Callback invoked whenever the selection changes. */
    private Consumer<Set<E>> onSelectionChanged;

    /**
     * Constructs a new EnumSetComboBox.
     *
     * @param enumClass The enum class.
     * @param initialSelection The initially selected enum constants (or null for empty).
     * @param iconProvider Function mapping enum constants to icons (can be null).
     * @param labelProvider Function mapping enum constants to labels (defaults to toString if null).
     * @param onSelectionChanged Callback invoked when selection is modified.
     */
    public EnumSetComboBox(
            @NonNull Class<E> enumClass,
            Set<E> initialSelection,
            Function<E, Icon> iconProvider,
            Function<E, String> labelProvider,
            Consumer<Set<E>> onSelectionChanged
    ) {
        this.enumClass = enumClass;
        this.iconProvider = iconProvider;
        this.labelProvider = labelProvider != null ? labelProvider : Enum::toString;
        this.onSelectionChanged = onSelectionChanged;

        if (initialSelection != null) {
            this.selectedValues.addAll(initialSelection);
        }

        setHorizontalAlignment(LEFT);
        setMargin(new Insets(2, 6, 2, 6));
        updateButtonDisplay();

        addActionListener(e -> showPopup());
    }

    /**
     * Updates the active selection and refreshes the button display.
     *
     * @param newSelection The new set of selected enum constants.
     */
    public void setSelectedValues(Set<E> newSelection) {
        this.selectedValues.clear();
        if (newSelection != null) {
            this.selectedValues.addAll(newSelection);
        }
        updateButtonDisplay();
    }

    /**
     * Sets a new listener for selection changes.
     *
     * @param onSelectionChanged The callback.
     */
    public void setOnSelectionChanged(Consumer<Set<E>> onSelectionChanged) {
        this.onSelectionChanged = onSelectionChanged;
    }

    /**
     * Updates the text and icon layout on the button face based on current selection.
     */
    private void updateButtonDisplay() {
        if (selectedValues.isEmpty()) {
            setText("None");
            setIcon(null);
            setToolTipText("No " + enumClass.getSimpleName() + " selected");
            return;
        }

        List<E> sorted = new ArrayList<>(selectedValues);
        sorted.sort(Enum::compareTo);

        String labelText = sorted.stream()
                .map(labelProvider)
                .collect(Collectors.joining(", "));

        setText(labelText);

        if (iconProvider != null && !sorted.isEmpty()) {
            setIcon(iconProvider.apply(sorted.get(0)));
        } else {
            setIcon(null);
        }

        setToolTipText("Selected: " + labelText);
    }

    /**
     * Opens the multi-selection checkbox popup menu beneath the button.
     */
    public void showPopup() {
        JPopupMenu popup = new JPopupMenu();
        popup.setBorder(BorderFactory.createLineBorder(new Color(180, 180, 180)));

        E[] allConstants = enumClass.getEnumConstants();
        for (E constant : allConstants) {
            JPanel row = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 2));
            row.setOpaque(false);

            JCheckBox cb = new JCheckBox();
            cb.setOpaque(false);
            cb.setSelected(selectedValues.contains(constant));

            JLabel textLabel = new JLabel(labelProvider.apply(constant));
            if (iconProvider != null) {
                Icon icon = iconProvider.apply(constant);
                if (icon != null) {
                    textLabel.setIcon(icon);
                }
            }

            row.add(cb);
            row.add(textLabel);

            cb.addActionListener(evt -> {
                if (cb.isSelected()) {
                    selectedValues.add(constant);
                } else {
                    selectedValues.remove(constant);
                }
                updateButtonDisplay();
                if (onSelectionChanged != null) {
                    onSelectionChanged.accept(new HashSet<>(selectedValues));
                }
            });

            row.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    cb.setSelected(!cb.isSelected());
                    cb.getActionListeners()[0].actionPerformed(null);
                }
            });

            JMenuItem menuItem = new JMenuItem();
            menuItem.setLayout(new BorderLayout());
            menuItem.add(row, BorderLayout.CENTER);
            menuItem.setPreferredSize(new Dimension(160, 26));
            popup.add(menuItem);
        }

        popup.show(this, 0, getHeight());
    }
}
