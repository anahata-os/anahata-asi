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
 * A programmatically drawn icon representing "Auto-Prune".
 * It combines a leaf with a circular arrow using Anahata brand colors.
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
        
        Color anahataGreen = new Color(40, 167, 69);
        Color anahataBlue = new Color(0, 123, 255);

        // Leaf part (Green)
        g2d.setColor(c.isEnabled() ? anahataGreen : Color.GRAY);
        Path2D leaf = new Path2D.Double();
        leaf.moveTo(x + size * 0.6, y + size * 0.8);
        leaf.curveTo(x + size * 0.95, y + size * 0.4, x + size * 0.6, y + size * 0.1, x + size * 0.6, y + size * 0.1);
        leaf.curveTo(x + size * 0.25, y + size * 0.4, x + size * 0.6, y + size * 0.8, x + size * 0.6, y + size * 0.8);
        g2d.fill(leaf);

        // Circular arrow part (Blue)
        g2d.setColor(c.isEnabled() ? anahataBlue : Color.GRAY);
        g2d.setStroke(new BasicStroke(size / 10f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        int r = size - 4;
        g2d.drawArc(x + 2, y + 2, r, r, 0, 270);

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
