/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details.
 */
package uno.anahata.ai.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing server-side tool execution.
 * It is stylized as a globe.
 *
 * @author pablo
 */
public class ServerToolsIcon implements Icon {

    private final int size;

    public ServerToolsIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

        // Base color: Green for server/remote tools
        Color baseColor = new Color(40, 167, 69);
        g2d.setColor(c.isEnabled() ? baseColor : Color.GRAY);

        double centerX = x + size / 2.0;
        double centerY = y + size / 2.0;
        double radius = size * 0.45;
        
        // Draw the globe outline
        g2d.setStroke(new BasicStroke(size * 0.05f));
        g2d.draw(new Ellipse2D.Double(centerX - radius, centerY - radius, radius * 2, radius * 2));

        // Draw the equator
        g2d.draw(new Line2D.Double(centerX - radius, centerY, centerX + radius, centerY));
        
        // Draw the meridians (vertical lines)
        double meridianOffset = radius * 0.5;
        g2d.draw(new Line2D.Double(centerX - meridianOffset, centerY - radius, centerX - meridianOffset, centerY + radius));
        g2d.draw(new Line2D.Double(centerX + meridianOffset, centerY - radius, centerX + meridianOffset, centerY + radius));
        
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
