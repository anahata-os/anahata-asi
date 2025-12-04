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
import java.awt.geom.Ellipse2D;
import java.awt.geom.Path2D;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing the local tool execution feature (Functions).
 * It is stylized as a gear.
 *
 * @author pablo
 */
public class LocalToolsIcon implements Icon {

    private final int size;

    public LocalToolsIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

        // Base color: Blue for local tools
        Color baseColor = new Color(0, 123, 255);
        g2d.setColor(c.isEnabled() ? baseColor : Color.GRAY);

        // Center point and radius
        double centerX = x + size / 2.0;
        double centerY = y + size / 2.0;
        double outerRadius = size * 0.45;
        double innerRadius = size * 0.2;
        
        // Draw the main gear shape
        Path2D gear = new Path2D.Double();
        int teeth = 8;
        double toothWidth = Math.PI / (teeth * 1.5);
        
        for (int i = 0; i < teeth; i++) {
            double angle = i * 2 * Math.PI / teeth;
            
            // Outer point of the tooth
            double x1 = centerX + outerRadius * Math.cos(angle);
            double y1 = centerY + outerRadius * Math.sin(angle);
            
            // Inner point of the tooth
            double x2 = centerX + outerRadius * Math.cos(angle + toothWidth);
            double y2 = centerY + outerRadius * Math.sin(angle + toothWidth);
            
            if (i == 0) {
                gear.moveTo(x1, y1);
            } else {
                gear.lineTo(x1, y1);
            }
            gear.lineTo(x2, y2);
        }
        gear.closePath();
        
        g2d.setStroke(new BasicStroke(size * 0.05f));
        g2d.draw(gear);
        
        // Draw the center hole
        g2d.setColor(c.getBackground());
        g2d.fill(new Ellipse2D.Double(centerX - innerRadius, centerY - innerRadius, innerRadius * 2, innerRadius * 2));
        
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
