/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Path2D;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing Java.
 * Stylized as a coffee cup with a Red fill, Blue outline, and Green steam.
 *
 * @author anahata
 */
public class JavaIcon implements Icon {

    private final int size;

    public JavaIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

        Color barcaRed = new Color(165, 0, 68);
        Color barcaBlue = new Color(0, 77, 152);
        Color anahataGreen = new Color(40, 167, 69);

        if (c.isEnabled()) {
            // Cup Fill (Red)
            g2d.setColor(barcaRed);
            g2d.fillRoundRect(x + size/4, y + size/2, size/2, size/3, 5, 5);

            // Cup Outline (Blue)
            g2d.setColor(barcaBlue);
            g2d.setStroke(new BasicStroke(size/12f));
            g2d.drawRoundRect(x + size/4, y + size/2, size/2, size/3, 5, 5);
            
            // Handle (Blue)
            g2d.drawArc(x + size*3/4 - 2, y + size/2 + 2, size/4, size/4, 270, 180);

            // Steam Waves (Green) - Irregular
            g2d.setColor(anahataGreen);
            g2d.setStroke(new BasicStroke(size/20f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            
            // Stroke 1 (Left)
            Path2D s1 = new Path2D.Double();
            s1.moveTo(x + size/2 - size/8, y + size/2 - 2);
            s1.curveTo(x + size/2 - size/10, y + size/3, x + size/2 - size/6, y + size/6, x + size/2 - size/12, y + 4);
            g2d.draw(s1);
            
            // Stroke 2 (Middle)
            Path2D s2 = new Path2D.Double();
            s2.moveTo(x + size/2, y + size/2 - 2);
            s2.curveTo(x + size/2 + size/12, y + size/3, x + size/2 - size/12, y + size/6, x + size/2, y + 2);
            g2d.draw(s2);
            
            // Stroke 3 (Right)
            Path2D s3 = new Path2D.Double();
            s3.moveTo(x + size/2 + size/8, y + size/2 - 2);
            s3.curveTo(x + size/2 + size/6, y + size/3, x + size/2 + size/10, y + size/6, x + size/2 + size/8, y + 6);
            g2d.draw(s3);
            
        } else {
            g2d.setColor(Color.GRAY);
            g2d.drawRoundRect(x + size/4, y + size/2, size/2, size/3, 5, 5);
        }

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
