/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing the "Save Session" action.
 * It is stylized as a classic floppy disk using the full Barça palette.
 *
 * @author anahata
 */
public class SaveSessionIcon implements Icon {

    private final int size;

    public SaveSessionIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

        Color barcaRed = new Color(165, 0, 68);
        Color barcaBlue = new Color(0, 77, 152);
        Color barcaYellow = new Color(255, 205, 0);

        if (c.isEnabled()) {
            // Disk body (Blue)
            g2d.setColor(barcaBlue);
            g2d.fillRoundRect(x + 2, y + 2, size - 4, size - 4, 2, 2);
            
            // Shutter (Red) - Top
            g2d.setColor(barcaRed);
            g2d.fillRect(x + size/4, y + 2, size/2, size/4);
            
            // Shutter hole (Blue)
            g2d.setColor(barcaBlue);
            g2d.fillRect(x + size/4 + 2, y + 4, 3, size/8);
            
            // Label (Yellow) - Bottom
            int lx = x + size/4;
            int ly = y + size/2;
            int lw = size/2;
            int lh = size/2 - 2;
            g2d.setColor(barcaYellow);
            g2d.fillRect(lx, ly, lw, lh);
            
            // "5-0" (Blue) centered on the label
            g2d.setColor(barcaBlue);
            Font font = new Font("SansSerif", Font.BOLD, size/4);
            g2d.setFont(font);
            FontMetrics fm = g2d.getFontMetrics(font);
            String text = "5-0";
            int tx = lx + (lw - fm.stringWidth(text)) / 2;
            int ty = ly + ((lh - fm.getHeight()) / 2) + fm.getAscent();
            g2d.drawString(text, tx, ty);
            
        } else {
            g2d.setColor(Color.GRAY);
            g2d.drawRect(x + 2, y + 2, size - 4, size - 4);
        }

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
