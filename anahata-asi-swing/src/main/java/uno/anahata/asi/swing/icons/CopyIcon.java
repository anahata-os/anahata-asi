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
 * A programmatically drawn icon representing a "Copy" action.
 * Stylized as two overlapping squares with the full Barça palette.
 *
 * @author anahata
 */
public class CopyIcon implements Icon {

    private final int size;

    public CopyIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

        Color barcaRed = new Color(165, 0, 68);
        Color barcaBlue = new Color(0, 77, 152);
        Color barcaYellow = new Color(255, 205, 0);

        int cw = size * 5 / 10;
        int ch = size * 5 / 10;
        int co = size * 3 / 10;

        if (c.isEnabled()) {
            // Back square (Blue)
            g2d.setColor(barcaBlue);
            g2d.drawRect(x + 2, y + 2, cw, ch);
            
            // Front square (Red)
            g2d.setColor(c.getBackground());
            g2d.fillRect(x + 2 + co, y + 2 + co, cw, ch);
            g2d.setColor(barcaRed);
            g2d.drawRect(x + 2 + co, y + 2 + co, cw, ch);
            
            // Yellow accent (inner line)
            g2d.setColor(barcaYellow);
            g2d.setStroke(new BasicStroke(1.0f));
            g2d.drawRect(x + 4 + co, y + 4 + co, cw - 4, ch - 4);
        } else {
            g2d.setColor(Color.GRAY);
            g2d.drawRect(x + 2, y + 2, cw, ch);
            g2d.drawRect(x + 2 + co, y + 2 + co, cw, ch);
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
