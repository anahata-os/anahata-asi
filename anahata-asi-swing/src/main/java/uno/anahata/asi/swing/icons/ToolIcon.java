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
 * A programmatic Swing icon for tools, representing an executable function.
 * It draws a "play" symbol inside a circular border.
 * 
 * @author anahata
 */
public class ToolIcon implements Icon {
    /** The size of the icon in pixels. */
    private final int size;
    /** The primary color of the icon. */
    private final Color color;

    /**
     * Constructs a new ToolIcon with the default size and color.
     * @param size The size in pixels.
     */
    public ToolIcon(int size) {
        this(size, new Color(0, 77, 152)); // Official FCB Blue
    }

    /**
     * Constructs a new ToolIcon with a specific size and color.
     * @param size The size in pixels.
     * @param color The color to use.
     */
    public ToolIcon(int size, Color color) {
        this.size = size;
        this.color = color;
    }

    /** {@inheritDoc} */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setColor(color);
        
        float strokeWidth = Math.max(1.0f, size / 16f);
        int padding = Math.max(1, size / 8);
        int innerSize = size - 2 * padding;
        
        // Draw circular border
        g2.setStroke(new BasicStroke(strokeWidth));
        g2.drawOval(x + padding, y + padding, innerSize, innerSize);
        
        // Draw "play" triangle
        int tw = innerSize / 2;
        int th = innerSize / 2;
        int tx = x + padding + (innerSize - tw) / 2 + (int)(strokeWidth * 1.5);
        int ty = y + padding + (innerSize - th) / 2;
        
        int[] px = {tx, tx + tw, tx};
        int[] py = {ty, ty + th / 2, ty + th};
        g2.fillPolygon(px, py, 3);
        
        g2.dispose();
    }

    /** {@inheritDoc} */
    @Override
    public int getIconWidth() {
        return size;
    }

    /** {@inheritDoc} */
    @Override
    public int getIconHeight() {
        return size;
    }
}
