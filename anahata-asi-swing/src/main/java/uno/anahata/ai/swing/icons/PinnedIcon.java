/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import javax.swing.Icon;

/**
 * A programmatically drawn pushpin icon to represent a "pinned" state.
 * The icon is twisted at a 45-degree angle with a Yellow body and Blue needle.
 *
 * @author anahata
 */
public class PinnedIcon implements Icon {

    private final int size;

    public PinnedIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        
        Color barcaRed = new Color(165, 0, 68);
        Color barcaBlue = new Color(0, 77, 152);
        Color barcaYellow = new Color(255, 205, 0);

        AffineTransform old = g2d.getTransform();
        g2d.rotate(Math.toRadians(45), x + size/2.0, y + size/2.0);

        if (c.isEnabled()) {
            // Head (Red)
            g2d.setColor(barcaRed);
            g2d.fillOval(x + size/4, y + 4, size/2, size/4);
            
            // Body (Yellow)
            g2d.setColor(barcaYellow);
            g2d.fillRoundRect(x + size/2 - size/8, y + size/4 + 2, size/4, size/2, 2, 2);
            
            // Needle (Blue)
            g2d.setColor(barcaBlue);
            g2d.setStroke(new BasicStroke(1.5f));
            g2d.drawLine(x + size/2, y + size*3/4, x + size/2, y + size - 2);
        } else {
            g2d.setColor(Color.GRAY);
            g2d.drawOval(x + size/4, y + 4, size/2, size/4);
            g2d.drawRect(x + size/2 - size/8, y + size/4 + 2, size/4, size/2);
        }

        g2d.setTransform(old);
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
