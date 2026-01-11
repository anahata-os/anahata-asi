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
 * A simple icon representing a grid of cards.
 * 
 * @author anahata-gemini-pro-2.5
 */
public class CardsIcon implements Icon {
    private final int size;

    public CardsIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setColor(c != null ? c.getForeground() : Color.BLACK);
        
        int padding = 2;
        int cardSize = (size - padding * 3) / 2;
        
        // Draw 4 small squares
        g2.drawRect(x + padding, y + padding, cardSize, cardSize);
        g2.drawRect(x + padding * 2 + cardSize, y + padding, cardSize, cardSize);
        g2.drawRect(x + padding, y + padding * 2 + cardSize, cardSize, cardSize);
        g2.drawRect(x + padding * 2 + cardSize, y + padding * 2 + cardSize, cardSize, cardSize);
        
        g2.dispose();
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
