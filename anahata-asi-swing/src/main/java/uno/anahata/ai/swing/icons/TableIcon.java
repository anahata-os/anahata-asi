/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import javax.swing.Icon;

/**
 * A simple icon representing a table.
 * 
 * @author anahata-gemini-pro-2.5
 */
public class TableIcon implements Icon {
    private final int size;

    public TableIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setColor(c != null ? c.getForeground() : Color.BLACK);
        
        int padding = 2;
        g2.drawRect(x + padding, y + padding, size - padding * 2, size - padding * 2);
        
        // Horizontal line (header)
        g2.drawLine(x + padding, y + padding + size / 3, x + size - padding, y + padding + size / 3);
        
        // Vertical lines
        g2.drawLine(x + padding + size / 3, y + padding, x + padding + size / 3, y + size - padding);
        g2.drawLine(x + padding + 2 * size / 3, y + padding, x + padding + 2 * size / 3, y + size - padding);
        
        g2.dispose();
    }

    @Override
    public int getIconWidth() {
        return size;
    }

    @Override
    public int getIconHeight() {
        return size;
    }
}
