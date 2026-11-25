/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import javax.swing.UIManager;

/**
 * A programmatically drawn Actual Size (1:1) icon badge.
 * <p>
 * Displays a crisp, anti-aliased "1:1" typographic badge representing resetting
 * zoom to 100% native unscaled resolution.
 * </p>
 *
 * @author anahata
 */
public class ActualSizeIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new ActualSizeIcon of the specified square size.
     *
     * @param size The size of the icon in pixels.
     */
    public ActualSizeIcon(int size) {
        super(size);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Paints a crisp, centered "1:1" label.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

            Color fg = UIManager.getColor("Button.foreground");
            if (fg == null) {
                fg = (c != null) ? c.getForeground() : Color.LIGHT_GRAY;
            }

            g2.setColor(c != null && !c.isEnabled() ? Color.GRAY : fg);
            g2.setFont(new Font("SansSerif", Font.BOLD, Math.max(9, (int) Math.round(size * 0.65))));

            FontMetrics fm = g2.getFontMetrics();
            String s = "1:1";
            int tx = x + (size - fm.stringWidth(s)) / 2;
            int ty = y + ((size - fm.getHeight()) / 2) + fm.getAscent();

            g2.drawString(s, tx, ty);
        } finally {
            g2.dispose();
        }
    }
}
