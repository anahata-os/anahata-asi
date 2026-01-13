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
 * @author anahata
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
        g2d.setStroke(new BasicStroke(2.2f));
        
        int m = 4;
        int l = 6;
        // Top Left
        g2d.drawLine(x+m, y+m, x+m+l, y+m);
        g2d.drawLine(x+m, y+m, x+m, y+m+l);
        // Top Right
        g2d.drawLine(x+size-m, y+m, x+size-m-l, y+m);
        g2d.drawLine(x+size-m, y+m, x+size-m, y+m+l);
        // Bottom Left
        g2d.drawLine(x+m, y+size-m, x+m+l, y+size-m);
        g2d.drawLine(x+m, y+size-m, x+m, y+size-m-l);
        // Bottom Right
        g2d.drawLine(x+size-m, y+size-m, x+size-m-l, y+size-m);
        g2d.drawLine(x+size-m, y+size-m, x+size-m, y+size-m-l);
        
        g2d.fillOval(x + size/2 - 2, y + size/2 - 2, 4, 4);
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
