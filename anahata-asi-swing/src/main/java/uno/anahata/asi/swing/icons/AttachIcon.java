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
 * A programmatically drawn Icon representing an "Attach" action.
 * Stylized as a classic paperclip using the Barça Blue.
 *
 * @author anahata
 */
public class AttachIcon implements Icon {

    private final int size;

    public AttachIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

        Color barcaBlue = new Color(0, 77, 152);
        
        float s = size;
        g2d.translate(x, y);
        
        if (c.isEnabled()) {
            g2d.setColor(barcaBlue);
        } else {
            g2d.setColor(Color.GRAY);
        }
        
        g2d.setStroke(new BasicStroke(s * 0.08f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        
        Path2D clip = new Path2D.Float();
        clip.moveTo(s * 0.75f, s * 0.35f);
        clip.lineTo(s * 0.75f, s * 0.7f);
        clip.curveTo(s * 0.75f, s * 0.9f, s * 0.25f, s * 0.9f, s * 0.25f, s * 0.7f);
        clip.lineTo(s * 0.25f, s * 0.25f);
        clip.curveTo(s * 0.25f, s * 0.05f, s * 0.65f, s * 0.05f, s * 0.65f, s * 0.25f);
        clip.lineTo(s * 0.65f, s * 0.65f);
        clip.curveTo(s * 0.65f, s * 0.75f, s * 0.35f, s * 0.75f, s * 0.35f, s * 0.65f);
        clip.lineTo(s * 0.35f, s * 0.35f);
        
        g2d.draw(clip);
        
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
