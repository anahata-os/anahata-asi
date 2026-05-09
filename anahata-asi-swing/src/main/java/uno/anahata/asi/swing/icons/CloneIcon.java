/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

/**
 * A super mega cool clone icon representing cellular mitosis or digital duplication.
 * 
 * @author anahata
 */
public class CloneIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new CloneIcon.
     * @param size The size of the icon.
     */
    public CloneIcon(int size) {
        super(size);
    }

    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        boolean enabled = c == null || c.isEnabled();
        Color strokeColor = enabled ? new Color(0, 150, 136) : Color.GRAY;
        Color fillColor1 = enabled ? new Color(0, 150, 136, 100) : new Color(150, 150, 150, 100);
        Color fillColor2 = enabled ? new Color(0, 200, 150, 150) : new Color(180, 180, 180, 150);
        int rectSize = (int) (size * 0.55);
        int offset = (int) (size * 0.2);
        int startX = x + (size - rectSize - offset) / 2;
        int startY = y + (size - rectSize - offset) / 2;
        g2.setStroke(new BasicStroke(1.5F));
        // Back rectangle
        g2.setColor(fillColor1);
        g2.fillRoundRect(startX + offset, startY, rectSize, rectSize, 4, 4);
        g2.setColor(strokeColor);
        g2.drawRoundRect(startX + offset, startY, rectSize, rectSize, 4, 4);
        // Front rectangle (filled)
        g2.setColor(fillColor2);
        g2.fillRoundRect(startX, startY + offset, rectSize, rectSize, 4, 4);
        g2.setColor(strokeColor);
        g2.drawRoundRect(startX, startY + offset, rectSize, rectSize, 4, 4);
        g2.dispose();
    }

}
