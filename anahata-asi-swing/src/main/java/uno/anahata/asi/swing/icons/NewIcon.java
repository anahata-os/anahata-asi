/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

/**
 * A sleek vector icon rendering a crisp 'NEW' badge.
 * 
 * @author anahata
 */
public class NewIcon extends AbstractAnahataIcon {

    /**
     * Constructs a NewIcon of the specified size.
     *
     * @param size The width and height in pixels.
     */
    public NewIcon(int size) {
        super(size);
    }

    /**
     * Constructs a default 16x16 NewIcon.
     */
    public NewIcon() {
        super(16);
    }

    /**
     * {@inheritDoc}
     * <p>Renders a rounded pill badge with the text 'NEW' centered inside.</p>
     */
    @Override
    public void paintIcon(Component c, java.awt.Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

            int pad = 1;
            int badgeW = size - (pad * 2);
            int badgeH = size - (pad * 2);

            // Fill badge pill
            g2.setColor(new Color(0, 122, 255)); // Vibrant Blue
            g2.fillRoundRect(x + pad, y + pad, badgeW, badgeH, 4, 4);

            // Draw text
            g2.setColor(Color.WHITE);
            Font font = new Font(Font.SANS_SERIF, Font.BOLD, Math.max(7, size - 7));
            g2.setFont(font);
            FontMetrics fm = g2.getFontMetrics(font);
            String text = "NEW";
            int textX = x + pad + (badgeW - fm.stringWidth(text)) / 2;
            int textY = y + pad + ((badgeH - fm.getHeight()) / 2) + fm.getAscent();
            g2.drawString(text, textX, textY);
        } finally {
            g2.dispose();
        }
    }
}
