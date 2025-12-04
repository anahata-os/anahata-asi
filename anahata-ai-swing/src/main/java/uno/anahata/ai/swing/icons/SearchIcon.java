/*
 * Licensed under the Anahata Software License (AS IS) v 108. See the LICENSE file for details. Fora Bara!
 */
package uno.anahata.ai.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import javax.swing.Icon;
import lombok.AllArgsConstructor;

/**
 * A programmatically drawn search (magnifying glass) icon.
 *
 * @author anahata-ai
 */
@AllArgsConstructor
public class SearchIcon implements Icon {

    private final int size;

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

        g2d.setColor(c.getForeground());
        g2d.setStroke(new BasicStroke(size / 12f));

        // Calculate dimensions for the magnifying glass
        int circleDiameter = size * 2 / 3;
        int circleX = x + (size - circleDiameter) / 2 - size / 10;
        int circleY = y + (size - circleDiameter) / 2 - size / 10;

        // Draw the circle part
        g2d.drawOval(circleX, circleY, circleDiameter, circleDiameter);

        // Draw the handle part
        int handleStartX = circleX + circleDiameter - 2;
        int handleStartY = circleY + circleDiameter - 2;
        int handleEndX = x + size - 1;
        int handleEndY = y + size - 1;

        g2d.drawLine(handleStartX, handleStartY, handleEndX, handleEndY);

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
