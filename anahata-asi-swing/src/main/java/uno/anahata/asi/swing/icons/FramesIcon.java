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
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing "Frames" or "Windows".
 * Stylized as overlapping windows using the Barça palette.
 *
 * @author anahata
 */
public class FramesIcon implements Icon {

    private final int size;

    public FramesIcon(int size) {
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
        float p = s * 0.12f; // Padding
        float w = s - (p * 2);
        float h = s - (p * 2);
        
        g2d.translate(x, y);
        
        if (c.isEnabled()) {
            g2d.setStroke(new BasicStroke(s * 0.06f));
            
            // Back frame (Blue)
            g2d.setColor(barcaBlue);
            g2d.draw(new Rectangle2D.Float(p, p, w * 0.7f, h * 0.7f));
            g2d.fill(new Rectangle2D.Float(p, p, w * 0.7f, h * 0.18f)); // Title bar
            
            // Front frame (Red)
            float fx = p + w * 0.3f;
            float fy = p + h * 0.3f;
            float fw = w * 0.7f;
            float fh = h * 0.7f;
            
            // Clear background for front frame
            g2d.setColor(c.getBackground());
            g2d.fill(new Rectangle2D.Float(fx, fy, fw, fh));
            
            g2d.setColor(barcaRed);
            g2d.draw(new Rectangle2D.Float(fx, fy, fw, fh));
            g2d.fill(new Rectangle2D.Float(fx, fy, fw, fh * 0.18f)); // Title bar
        } else {
            g2d.setColor(Color.GRAY);
            g2d.draw(new Rectangle2D.Float(p, p, w * 0.7f, h * 0.7f));
            g2d.draw(new Rectangle2D.Float(p + w * 0.3f, p + h * 0.3f, w * 0.7f, h * 0.7f));
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
