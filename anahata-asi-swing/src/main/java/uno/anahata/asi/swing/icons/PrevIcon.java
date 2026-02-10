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
 * A programmatically drawn Previous (left arrow/chevron) icon.
 * Stylized with Barca Blue.
 * 
 * @author anahata
 */
public class PrevIcon implements Icon {
    private final int size;

    public PrevIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

        Color barcaBlue = new Color(0, 77, 152);
        g2d.setColor(c.isEnabled() ? barcaBlue : Color.GRAY);
        
        float thickness = size / 6f;
        g2d.setStroke(new BasicStroke(thickness, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));

        Path2D path = new Path2D.Double();
        path.moveTo(x + size * 0.7, y + size * 0.2);
        path.lineTo(x + size * 0.3, y + size * 0.5);
        path.lineTo(x + size * 0.7, y + size * 0.8);
        
        g2d.draw(path);
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
