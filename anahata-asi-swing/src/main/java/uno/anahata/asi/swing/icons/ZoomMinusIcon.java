/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import javax.swing.UIManager;

/**
 * A programmatically drawn Zoom-Out (magnifying glass with minus) icon.
 * <p>
 * Displays a vector magnifying glass containing a minus sign.
 * </p>
 *
 * @author anahata
 */
public class ZoomMinusIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new ZoomMinusIcon of the specified square size.
     *
     * @param size The size of the icon in pixels.
     */
    public ZoomMinusIcon(int size) {
        super(size);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Paints a magnifying glass with a horizontal minus stroke inside.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            Color fg = UIManager.getColor("Button.foreground");
            if (fg == null) {
                fg = (c != null) ? c.getForeground() : Color.LIGHT_GRAY;
            }

            g2.setColor(c != null && !c.isEnabled() ? Color.GRAY : fg);
            g2.setStroke(new BasicStroke(1.4f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));

            int r = size - 7;
            g2.drawOval(x + 1, y + 1, r, r);

            // Handle
            g2.drawLine(x + r - 1, y + r - 1, x + size - 2, y + size - 2);

            // Minus sign inside lens
            int midY = y + 1 + r / 2;
            int midX = x + 1 + r / 2;
            int arm = Math.max(2, r / 3);
            g2.drawLine(midX - arm, midY, midX + arm, midY);
        } finally {
            g2.dispose();
        }
    }
}
