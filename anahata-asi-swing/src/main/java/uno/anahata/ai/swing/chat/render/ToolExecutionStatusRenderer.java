/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.Component;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JList;
import uno.anahata.ai.model.tool.ToolExecutionStatus;
import uno.anahata.ai.swing.chat.SwingChatConfig;

/**
 * A specialized renderer for {@link ToolExecutionStatus} enums in JComboBoxes.
 * It displays the status name with its corresponding color from the theme.
 * 
 * @author anahata-ai
 */
public class ToolExecutionStatusRenderer extends DefaultListCellRenderer {

    @Override
    public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
        // For the JComboBox button (index == -1), we don't want the selection background
        boolean isComboBoxButton = index == -1;
        Component c = super.getListCellRendererComponent(list, value, index, isSelected && !isComboBoxButton, cellHasFocus);
        
        if (value instanceof ToolExecutionStatus status) {
            setText(status.name());
            setForeground(SwingChatConfig.getColor(status));
        }
        
        return c;
    }
}
