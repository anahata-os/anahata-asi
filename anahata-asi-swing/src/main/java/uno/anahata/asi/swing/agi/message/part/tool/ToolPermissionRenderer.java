/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.message.part.tool;

import java.awt.Component;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JList;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.swing.agi.SwingAgiConfig;

/**
 * A specialized renderer for {@link ToolPermission} enums in JComboBoxes.
 * It displays the human-readable display value of the permission with its corresponding color.
 * 
 * @author anahata-ai
 */
public class ToolPermissionRenderer extends DefaultListCellRenderer {

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Renders the human-readable display value 
     * and applies the color coding corresponding to the permission level 
     * (e.g., Green for Always, Red for Never).</p>
     */
    @Override
    public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
        // For the JComboBox button (index == -1), we don't want the selection background
        boolean isComboBoxButton = index == -1;
        Component c = super.getListCellRendererComponent(list, value, index, isSelected && !isComboBoxButton, cellHasFocus);
        
        if (value instanceof ToolPermission tp) {
            setText(tp.getDisplayValue());
            setForeground(SwingAgiConfig.getColor(tp));
        }
        
        return c;
    }
}
