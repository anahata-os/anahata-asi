/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Path2D;
import javax.swing.Icon;

/**
 * A programmatically drawn Send icon (Paper Plane).
 * Stylized with Barca Blue.
 * 
 * @author anahata
 */
public class SendIcon implements Icon {
    private final int size;

    public SendIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        Color barcaBlue = new Color(0, 77, 152);
        g2d.setColor(c.isEnabled() ? barcaBlue : Color.GRAY);

        Path2D plane = new Path2D.Double();
        // Simple paper plane path
        plane.moveTo(x + size * 0.1, y + size * 0.5);
        plane.lineTo(x + size * 0.9, y + size * 0.2);
        plane.lineTo(x + size * 0.4, y + size * 0.8);
        plane.lineTo(x + size * 0.4, y + size * 0.5);
        plane.closePath();
        
        g2d.fill(plane);
        
        // Draw the center fold
        g2d.setColor(Color.WHITE);
        g2d.drawLine((int)(x + size * 0.4), (int)(y + size * 0.5), (int)(x + size * 0.9), (int)(y + size * 0.2));

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
