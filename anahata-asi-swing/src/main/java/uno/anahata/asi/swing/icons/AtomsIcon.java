/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Ellipse2D;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing Toolkits (Atoms).
 * Stylized as four orbiting circles using Anahata Green.
 */
public class AtomsIcon implements Icon {
    private final int size;

    public AtomsIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setColor(new Color(40, 167, 69)); // Green
        
        double center = size / 2.0;
        double radius = size / 4.0;
        
        for (int i = 0; i < 4; i++) {
            double angle = i * Math.PI / 2.0;
            double cx = center + radius * Math.cos(angle);
            double cy = center + radius * Math.sin(angle);
            g2.fill(new Ellipse2D.Double(x + cx - 2, y + cy - 2, 4, 4));
        }
        g2.fill(new Ellipse2D.Double(x + center - 2, y + center - 2, 4, 4));
        g2.dispose();
    }

    @Override public int getIconWidth() { return size; }
    @Override public int getIconHeight() { return size; }
}
