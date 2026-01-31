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
import java.awt.geom.Path2D;
import javax.swing.Icon;

/**
 * A refined programmatic Swing icon for Java toolkits.
 * It draws a bold, stylized coffee cup with a saucer and steam.
 * 
 * @author anahata
 */
public class JavaIconRefined implements Icon {
    /** The size of the icon in pixels. */
    private final int size;
    /** The primary color of the icon. */
    private final Color color;

    /**
     * Constructs a new JavaIconRefined with the default size and color.
     * @param size The size in pixels.
     */
    public JavaIconRefined(int size) {
        this(size, new Color(165, 0, 68)); // Official FCB Red
    }

    /**
     * Constructs a new JavaIconRefined with a specific size and color.
     * @param size The size in pixels.
     * @param color The color to use.
     */
    public JavaIconRefined(int size, Color color) {
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
        
        // 1. Saucer (Bottom Ellipse)
        int sw = (int)(size * 0.8);
        int sh = (int)(size * 0.15);
        int sx = x + (size - sw) / 2;
        int sy = y + size - sh - (int)(size * 0.05);
        g2.fillOval(sx, sy, sw, sh);
        
        // 2. Cup Body (Rounded Trapezoid)
        int cw = (int)(size * 0.55);
        int ch = (int)(size * 0.45);
        int cx = x + (size - cw) / 2 - (int)(size * 0.05);
        int cy = sy - ch + (int)(sh * 0.5);
        
        Path2D cup = new Path2D.Double();
        cup.moveTo(cx, cy);
        cup.lineTo(cx + cw, cy);
        cup.lineTo(cx + cw * 0.85, cy + ch);
        cup.lineTo(cx + cw * 0.15, cy + ch);
        cup.closePath();
        g2.fill(cup);
        
        // 3. Handle
        g2.setStroke(new BasicStroke(size/10f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        g2.drawArc(cx + cw - size/8, cy + size/12, size/4, size/4, -90, 180);
        
        // 4. Steam (Two bold lines)
        g2.setStroke(new BasicStroke(size/12f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        // Left steam
        Path2D s1 = new Path2D.Double();
        s1.moveTo(cx + cw * 0.3, cy - size * 0.05);
        s1.curveTo(cx + cw * 0.4, cy - size * 0.15, cx + cw * 0.2, cy - size * 0.25, cx + cw * 0.3, cy - size * 0.35);
        g2.draw(s1);
        // Right steam
        Path2D s2 = new Path2D.Double();
        s2.moveTo(cx + cw * 0.6, cy - size * 0.05);
        s2.curveTo(cx + cw * 0.7, cy - size * 0.15, cx + cw * 0.5, cy - size * 0.25, cx + cw * 0.6, cy - size * 0.35);
        g2.draw(s2);
        
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
        return new JavaIconRefined(s, color);
    }
}
