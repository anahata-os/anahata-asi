/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Path2D;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing the V2 Resources (Prism).
 * Stylized as a diamond/prism shape using the Barça orange.
 */
public class PrismIcon implements Icon {
    private final int size;

    public PrismIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setColor(new Color(0xFF9800)); // Orange
        
        Path2D p = new Path2D.Double();
        p.moveTo(x + size / 2, y + 2);
        p.lineTo(x + size - 2, y + size / 2);
        p.lineTo(x + size / 2, y + size - 2);
        p.lineTo(x + 2, y + size / 2);
        p.closePath();
        g2.fill(p);
        g2.dispose();
    }

    @Override public int getIconWidth() { return size; }
    @Override public int getIconHeight() { return size; }
}
