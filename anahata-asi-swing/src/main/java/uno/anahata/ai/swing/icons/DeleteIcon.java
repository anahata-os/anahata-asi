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
import java.awt.geom.Path2D;
import javax.swing.Icon;

/**
 * A programmatically drawn trash can icon for delete actions.
 * Stylized with a Red body, Yellow stripe, Blue lid, and a Red handle.
 *
 * @author anahata
 */
public class DeleteIcon implements Icon {

    private final int size;

    public DeleteIcon(int size) {
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

        if (c.isEnabled()) {
            // Tapered body (Red)
            int[] px = {x + size/4, x + size*3/4, x + size*2/3, x + size/3};
            int[] py = {y + size/3, y + size/3, y + size - 2, y + size - 2};
            g2d.setColor(barcaRed);
            g2d.fillPolygon(px, py, 4);
            
            // Yellow stripe on the body
            g2d.setColor(barcaYellow);
            g2d.fillRect(x + size/3 + 2, y + size/2, size/3 - 4, 2);

            // Lid (Blue)
            g2d.setColor(barcaBlue);
            g2d.fillRoundRect(x + 2, y + size/4, size - 4, size/10 + 1, 2, 2);
            
            // Boina / Handle (Red)
            g2d.setColor(barcaRed);
            g2d.fillRect(x + size/2 - 3, y + size/8, 6, 3);
            
        } else {
            g2d.setColor(Color.GRAY);
            g2d.drawRect(x + size/4, y + size/3, size/2, size*2/3 - 2);
            g2d.drawRect(x + 2, y + size/4, size - 4, size/10);
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
