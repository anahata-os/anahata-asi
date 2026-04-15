/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.provider;

import java.awt.Component;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JList;
import uno.anahata.asi.agi.provider.AbstractModel;

/**
 * A standard cell renderer for {@link AbstractModel} objects.
 * Shows the model's display name.
 * 
 * @author anahata
 */
public class ModelRenderer extends DefaultListCellRenderer {
    @Override
    public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
        super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        if (value instanceof AbstractModel m) {
            setText(m.getDisplayName());
        }
        return this;
    }
}
