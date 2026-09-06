/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

/**
 * A sleek vector icon rendering a crisp 'NEW' badge with comfortable horizontal padding.
 * 
 * @author anahata
 */
public class NewIcon extends AbstractAnahataIcon {

    /**
     * Constructs a NewIcon of the specified size.
     *
     * @param size The height in pixels.
     */
    public NewIcon(int size) {
        super(size);
    }

    /**
     * Constructs a default 14px NewIcon.
     */
    public NewIcon() {
        super(14);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns an elongated width to ensure the 'NEW' text has ample blue padding on both sides.
     * </p>
     */
    @Override
    public int getIconWidth() {
        return (int) Math.round(size * 1.8);
    }

    /**
     * {@inheritDoc}
     * <p>Renders a rounded pill badge with vibrant blue background and 'NEW' centered inside.</p>
     */
    @Override
    public void paintIcon(Component c, java.awt.Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

            int width = getIconWidth();
            int height = getIconHeight();
            int padY = 1;
            int badgeH = height - (padY * 2);
            int badgeW = width - 2;

            // Fill badge pill with vibrant blue
            g2.setColor(new Color(0, 122, 255));
            g2.fillRoundRect(x + 1, y + padY, badgeW, badgeH, 4, 4);

            // Draw bold white text centered with padding
            g2.setColor(Color.WHITE);
            int fontSize = Math.max(8, size - 5);
            Font font = new Font(Font.SANS_SERIF, Font.BOLD, fontSize);
            g2.setFont(font);
            FontMetrics fm = g2.getFontMetrics(font);
            String text = "NEW";
            int textX = x + 1 + (badgeW - fm.stringWidth(text)) / 2;
            int textY = y + padY + ((badgeH - fm.getHeight()) / 2) + fm.getAscent();
            g2.drawString(text, textX, textY);
        } finally {
            g2.dispose();
        }
    }
}
