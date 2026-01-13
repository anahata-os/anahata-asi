package uno.anahata.asi.swing.media.util;

import java.awt.Component;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;

/**
 * Custom ListCellRenderer for LineInfo to display human-readable names.
 */
public class LineInfoRenderer extends DefaultListCellRenderer {
    @Override
    public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
        super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        if (value instanceof LineInfo) {
            LineInfo lineInfo = (LineInfo) value;
            setText(lineInfo.toString());
        }
        return this;
    }
}
