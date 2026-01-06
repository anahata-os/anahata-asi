/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.Component;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;
import uno.anahata.ai.model.tool.ToolPermission;

/**
 * A specialized renderer for {@link ToolPermission} enums in JComboBoxes.
 * It displays the human-readable display value of the permission.
 * 
 * @author anahata-ai
 */
public class ToolPermissionRenderer extends DefaultListCellRenderer {

    @Override
    public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
        super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        if (value instanceof ToolPermission tp) {
            setText(tp.getDisplayValue());
        }
        return this;
    }
}
