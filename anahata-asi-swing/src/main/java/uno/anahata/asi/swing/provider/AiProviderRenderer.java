/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.provider;

import java.awt.Component;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.JList;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * A standard cell renderer for {@link AbstractAiProvider} objects.
 * Shows the display name and the UUID in parentheses.
 * 
 * @author anahata
 */
public class AiProviderRenderer extends DefaultListCellRenderer {
    @Override
    public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
        super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        if (value instanceof AbstractAiProvider p) {
            setText(p.getDisplayName());
            Icon icon = IconUtils.getIcon("aiproviders/" + p.getClass().getName() + ".png", 16, 16);
            setIcon(icon);
        }
        return this;
    }
}
