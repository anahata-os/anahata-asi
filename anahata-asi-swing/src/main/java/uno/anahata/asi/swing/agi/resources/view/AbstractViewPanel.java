/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources.view;

import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.view.ResourceView;

/**
 * Common base for view-specific metadata panels.
 * <p>
 * This class provides the standard sectored layout for semantic interpreter metadata.
 * It is managed by the {@code Resource2Panel} which authoritatively swaps 
 * these panels based on the resource type.
 * </p>
 * 
 * @param <V> The view type this panel renders.
 * @author anahata
 */
@Slf4j
public abstract class AbstractViewPanel<V extends ResourceView> extends JPanel {

    /** The current view being displayed. */
    @Getter
    protected V view;

    /** The layout constraints for property labels/fields. */
    protected final GridBagConstraints gbc = new GridBagConstraints();

    /**
     * Constructs a new view panel with a standard GridBag layout.
     */
    protected AbstractViewPanel() {
        setLayout(new GridBagLayout());
        setBorder(BorderFactory.createTitledBorder(null, "View (Interpreter)", TitledBorder.LEFT, TitledBorder.TOP, getFont().deriveFont(Font.BOLD)));
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 5, 2, 5);
        gbc.fill = GridBagConstraints.NONE;
    }

    /**
     * Sets the view to display and triggers a UI refresh.
     * @param view The view instance.
     */
    public final void setView(V view) {
        this.view = view;
        if (view != null) {
            refresh();
        }
    }

    /**
     * Helper to add a labelled property to the grid.
     * @param label The property label.
     * @param component The value component.
     */
    protected void addProperty(String label, java.awt.Component component) {
        gbc.gridx = 0;
        gbc.weightx = 0;
        add(new JLabel(label), gbc);
        
        gbc.gridx = 1;
        gbc.weightx = 1.0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        add(component, gbc);
        
        gbc.gridy++;
        gbc.fill = GridBagConstraints.NONE;
    }

    /**
     * Refreshes the UI components with the current view's metadata. 
     * <p>
     * <b>Access Authority:</b> This method is public to allow the Resource2Panel 
     * to trigger refreshes when the underlying resource state changes.
     * </p>
     */
    public abstract void refresh();
}
