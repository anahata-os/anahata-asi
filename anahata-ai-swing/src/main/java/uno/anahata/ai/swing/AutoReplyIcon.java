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
 * A programmatically drawn Icon representing the auto-reply feature.
 * It is stylized as a "fast forward" symbol (two right-pointing triangles).
 *
 * @author pablo
 */
public class AutoReplyIcon implements Icon {

    private final int size;

    public AutoReplyIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

        // Base color: Amber/Yellow for attention/notification
        Color baseColor = new Color(255, 193, 7);
        g2d.setColor(c.isEnabled() ? baseColor : Color.GRAY);

        double triangleHeight = size * 0.6;
        double triangleWidth = size * 0.3;
        double startX = x + size * 0.2;
        double centerY = y + size / 2.0;

        // First triangle
        Path2D triangle1 = new Path2D.Double();
        triangle1.moveTo(startX, centerY - triangleHeight / 2);
        triangle1.lineTo(startX + triangleWidth, centerY);
        triangle1.lineTo(startX, centerY + triangleHeight / 2);
        triangle1.closePath();
        g2d.fill(triangle1);

        // Second triangle
        double secondTriangleX = startX + triangleWidth + size * 0.05;
        Path2D triangle2 = new Path2D.Double();
        triangle2.moveTo(secondTriangleX, centerY - triangleHeight / 2);
        triangle2.lineTo(secondTriangleX + triangleWidth, centerY);
        triangle2.lineTo(secondTriangleX, centerY + triangleHeight / 2);
        triangle2.closePath();
        g2d.fill(triangle2);
        
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
