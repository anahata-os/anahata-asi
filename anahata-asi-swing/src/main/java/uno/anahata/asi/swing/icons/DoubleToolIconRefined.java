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
 * A refined programmatic Swing icon for tool containers (ToolsNode).
 * It draws a "fast-forward" symbol (two triangles) without a border.
 * 
 * @author anahata
 */
public class DoubleToolIconRefined implements Icon {
    /** The size of the icon in pixels. */
    private final int size;
    /** The primary color of the icon. */
    private final Color color;

    /**
     * Constructs a new DoubleToolIconRefined with the default size and color.
     * @param size The size in pixels.
     */
    public DoubleToolIconRefined(int size) {
        this(size, new Color(0, 77, 152)); // Official FCB Blue
    }

    /**
     * Constructs a new DoubleToolIconRefined with a specific size and color.
     * @param size The size in pixels.
     * @param color The color to use.
     */
    public DoubleToolIconRefined(int size, Color color) {
        this.size = size;
        this.color = color;
    }

    /** {@inheritDoc} */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);
        g2.setColor(color);
        
        int tw = size / 3;
        int th = (int)(size * 0.6);
        int ty = y + (size - th) / 2;
        int tx = x + (size - (int)(tw * 1.8)) / 2;
        
        int[] x1 = {tx, tx + tw, tx};
        int[] y1 = {ty, ty + th/2, ty + th};
        g2.fillPolygon(x1, y1, 3);
        
        int[] x2 = {tx + (int)(tw * 0.8), tx + (int)(tw * 1.8), tx + (int)(tw * 0.8)};
        g2.fillPolygon(x2, y1, 3);
        
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

    /**
     * Creates a new instance of this icon with a different size.
     * @param s The new size.
     * @return The derived icon.
     */
    public Icon derive(int s) {
        return new DoubleToolIconRefined(s, color);
    }
}
