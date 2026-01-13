/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing a single "Run" action.
 * Stylized as a single play button (triangle).
 *
 * @author anahata-ai
 */
public class RunIcon implements Icon {

    private final int size;

    public RunIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        
        Color anahataBlue = new Color(0, 123, 255);
        g2d.setColor(c.isEnabled() ? anahataBlue : Color.GRAY);

        int tw = size / 2;
        int th = size / 2;
        int tx = x + (size - tw) / 2;
        int ty = y + (size - th) / 2;
        
        int[] xPoints = {tx, tx + tw, tx};
        int[] yPoints = {ty, ty + th/2, ty + th};
        g2d.fillPolygon(xPoints, yPoints, 3);
        
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
