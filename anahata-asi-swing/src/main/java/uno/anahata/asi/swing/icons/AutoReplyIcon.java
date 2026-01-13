/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing the auto-reply feature.
 * It is stylized as a "fast forward" symbol using Anahata brand colors.
 *
 * @author anahata
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

        Color anahataBlue = new Color(0, 123, 255);
        Color anahataGreen = new Color(40, 167, 69);

        g2d.setColor(c.isEnabled() ? anahataBlue : Color.GRAY);

        int tw = size / 3;
        int th = size / 2;
        int ty = y + (size - th) / 2;
        
        int[] x1 = {x + 4, x + 4 + tw, x + 4};
        int[] y1 = {ty, ty + th/2, ty + th};
        g2d.fillPolygon(x1, y1, 3);
        
        int[] x2 = {x + 4 + tw, x + 4 + tw*2, x + 4 + tw};
        g2d.fillPolygon(x2, y1, 3);
        
        g2d.setColor(c.isEnabled() ? anahataGreen : Color.GRAY);
        g2d.setFont(new Font("SansSerif", Font.BOLD, size/2));
        g2d.drawString("A", x + size - size/3, y + size - 2);
        
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
