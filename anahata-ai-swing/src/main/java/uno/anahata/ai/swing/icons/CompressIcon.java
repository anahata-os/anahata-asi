/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details.
 */
package uno.anahata.ai.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing a context compression action.
 * It is stylized as four arrows pointing inwards.
 *
 * @author pablo
 */
public class CompressIcon implements Icon {
    private final int size;

    public CompressIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setColor(c.isEnabled() ? new Color(128, 0, 128) : Color.GRAY);
        g2d.setStroke(new BasicStroke(size * 0.1f));
        
        int centerX = x + size / 2;
        int centerY = y + size / 2;
        int margin = (int)(size * 0.15);
        
        // Top-left arrow
        g2d.drawLine(x + margin, y + margin, centerX - 2, centerY - 2);
        // Top-right arrow
        g2d.drawLine(x + size - margin, y + margin, centerX + 2, centerY - 2);
        // Bottom-left arrow
        g2d.drawLine(x + margin, y + size - margin, centerX - 2, centerY + 2);
        // Bottom-right arrow
        g2d.drawLine(x + size - margin, y + size - margin, centerX + 2, centerY + 2);
        
        g2d.dispose();
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
