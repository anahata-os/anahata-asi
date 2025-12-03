/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details.
 */
package uno.anahata.ai.swing;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Path2D;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing a restart or clear action.
 * It is stylized as a circular arrow.
 *
 * @author pablo
 */
public class RestartIcon implements Icon {
    private final int size;

    public RestartIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setColor(c.isEnabled() ? new Color(0, 100, 0) : Color.GRAY);
        g2d.setStroke(new BasicStroke(size * 0.12f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        
        int arcSize = (int)(size * 0.7);
        int arcX = x + (size - arcSize) / 2;
        int arcY = y + (size - arcSize) / 2;
        g2d.drawArc(arcX, arcY, arcSize, arcSize, 30, 300);
        
        // Arrow head
        Path2D.Double arrowHead = new Path2D.Double();
        int arrowBaseX = x + size - (int)(size * 0.35);
        int arrowBaseY = y + size - (int)(size * 0.35);
        arrowHead.moveTo(arrowBaseX, arrowBaseY - (int)(size * 0.2));
        arrowHead.lineTo(arrowBaseX + (int)(size * 0.2), arrowBaseY);
        arrowHead.lineTo(arrowBaseX, arrowBaseY + (int)(size * 0.2));
        g2d.fill(arrowHead);
        
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
