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
import java.awt.geom.Ellipse2D;
import java.awt.geom.Path2D;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing the local tool execution feature (Functions).
 * It is stylized as a gear.
 *
 * @author anahata
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

        double cx = x + size / 2.0;
        double cy = y + size / 2.0;
        double rOut = size * 0.42;
        double rIn = size * 0.28;
        
        Path2D gear = new Path2D.Double();
        int teeth = 8;
        for (int i = 0; i < teeth; i++) {
            double a = i * 2 * Math.PI / teeth;
            double x1 = cx + rOut * Math.cos(a - 0.2);
            double y1 = cy + rOut * Math.sin(a - 0.2);
            double x2 = cx + rOut * Math.cos(a + 0.2);
            double y2 = cy + rOut * Math.sin(a + 0.2);
            
            if (i == 0) gear.moveTo(x1, y1);
            else gear.lineTo(x1, y1);
            
            gear.lineTo(x2, y2);
            
            double mid = a + Math.PI / teeth;
            gear.lineTo(cx + rIn * Math.cos(mid), cy + rIn * Math.sin(mid));
        }
        gear.closePath();
        g2d.fill(gear);
        
        g2d.setColor(c.getBackground());
        g2d.fill(new Ellipse2D.Double(cx - size*0.12, cy - size*0.12, size*0.24, size*0.24));
        
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
