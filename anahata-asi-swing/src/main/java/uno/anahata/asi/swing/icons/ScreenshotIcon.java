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
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing a "Screenshot" action.
 * Stylized as a monitor using the Barça palette.
 *
 * @author anahata
 */
public class ScreenshotIcon implements Icon {

    private final int size;

    public ScreenshotIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

        Color barcaRed = new Color(165, 0, 68);
        Color barcaBlue = new Color(0, 77, 152);
        
        float s = size;
        float p = s * 0.15f; // Padding
        float w = s - (p * 2);
        float h = s - (p * 2);
        
        g2d.translate(x, y);
        
        if (c.isEnabled()) {
            g2d.setStroke(new BasicStroke(s * 0.08f));
            
            // Screen (Blue)
            g2d.setColor(barcaBlue);
            g2d.draw(new RoundRectangle2D.Float(p, p, w, h * 0.65f, s * 0.1f, s * 0.1f));
            
            // Stand (Red)
            g2d.setColor(barcaRed);
            g2d.fill(new Rectangle2D.Float(s * 0.42f, s * 0.7f, s * 0.16f, s * 0.1f)); // Neck
            g2d.fill(new Rectangle2D.Float(s * 0.3f, s * 0.8f, s * 0.4f, s * 0.08f)); // Base
        } else {
            g2d.setColor(Color.GRAY);
            g2d.draw(new RoundRectangle2D.Float(p, p, w, h * 0.65f, s * 0.1f, s * 0.1f));
            g2d.fillRect((int)(s * 0.3f), (int)(s * 0.8f), (int)(s * 0.4f), (int)(s * 0.08f));
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
