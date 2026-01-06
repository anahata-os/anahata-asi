/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.Color;
import java.awt.Component;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JLabel;
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
        super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        if (value instanceof ToolExecutionStatus status) {
            setText(status.name());
            if (!isSelected) {
                setForeground(Color.decode(SwingChatConfig.getColor(status)));
            }
        }
        return this;
    }

    private String toHexString(Color color) {
        return String.format("#%02x%02x%02x", color.getRed(), color.getGreen(), color.getBlue());
    }
}
