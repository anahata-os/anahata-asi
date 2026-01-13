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
 * A programmatically drawn icon representing "Auto-Prune".
 * Stylized as a banana peel—the universal symbol for "slipping" things out of context.
 * Uses Barça Yellow for the peel.
 *
 * @author anahata
 */
public class AutoPruneIcon implements Icon {

    private final int size;

    public AutoPruneIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        
        Color barcaYellow = new Color(255, 205, 0);
        Color peelShadow = new Color(200, 160, 0);

        if (c.isEnabled()) {
            // The Banana Peel
            g2d.setColor(barcaYellow);
            
            // Center hub
            double cx = x + size / 2.0;
            double cy = y + size / 2.0;
            
            // Three prongs of the peel
            for (int i = 0; i < 3; i++) {
                g2d.rotate(Math.toRadians(120), cx, cy);
                Path2D prong = new Path2D.Double();
                prong.moveTo(cx, cy);
                prong.curveTo(cx - size * 0.2, cy + size * 0.1, cx - size * 0.4, cy + size * 0.4, cx - size * 0.1, cy + size * 0.45);
                prong.curveTo(cx + size * 0.1, cy + size * 0.3, cx + size * 0.1, cy + size * 0.1, cx, cy);
                g2d.fill(prong);
                
                g2d.setColor(peelShadow);
                g2d.setStroke(new BasicStroke((float) (size * 0.02)));
                g2d.draw(prong);
                g2d.setColor(barcaYellow);
            }
            
            // The "stem" bit at the top
            g2d.setColor(new Color(101, 67, 33)); // Brown stem
            g2d.fillOval((int)(cx - size*0.05), (int)(cy - size*0.05), (int)(size*0.1), (int)(size*0.1));

        } else {
            g2d.setColor(Color.GRAY);
            g2d.drawOval(x + 2, y + 2, size - 4, size - 4);
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
