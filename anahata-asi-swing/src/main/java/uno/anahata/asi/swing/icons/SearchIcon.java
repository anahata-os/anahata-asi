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
import javax.swing.Icon;

/**
 * A programmatically drawn search (magnifying glass) icon using the full Barça palette.
 * Features concentric Blue, Yellow, and Green circles.
 *
 * @author anahata-ai
 */
public class SearchIcon implements Icon {

    private final int size;

    public SearchIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

        Color barcaBlue = new Color(0, 77, 152);
        Color barcaYellow = new Color(255, 205, 0);
        Color anahataGreen = new Color(40, 167, 69);

        int cd = (int)(size * 0.6);
        int cx = x + 2;
        int cy = y + 2;

        if (c.isEnabled()) {
            // Outer circle (Blue)
            g2d.setColor(barcaBlue);
            g2d.fillOval(cx, cy, cd, cd);
            
            // Middle circle (Yellow)
            g2d.setColor(barcaYellow);
            int mSize = cd - size/6;
            g2d.fillOval(cx + size/12, cy + size/12, mSize, mSize);
            
            // Lens (Green)
            g2d.setColor(anahataGreen);
            int iSize = cd - size/3;
            g2d.fillOval(cx + size/6, cy + size/6, iSize, iSize);
            
            // Handle (Blue) - Extended and spaced
            g2d.setColor(barcaBlue);
            g2d.setStroke(new BasicStroke(size/6f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            double startOffset = size * 0.6 + 2;
            double endOffset = size * 0.82;
            g2d.drawLine((int)(x + startOffset), (int)(y + startOffset), (int)(x + endOffset), (int)(y + endOffset));
        } else {
            g2d.setColor(Color.GRAY);
            g2d.fillOval(cx, cy, cd, cd);
            g2d.setStroke(new BasicStroke(size/6f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            g2d.drawLine(cx + cd - 2, cy + cd - 2, x + size - 2, y + size - 2);
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
