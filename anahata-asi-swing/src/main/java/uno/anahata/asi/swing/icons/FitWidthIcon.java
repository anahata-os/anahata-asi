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
 * A programmatically drawn Fit-Width document icon.
 * <p>
 * Displays a document page silhouette with a horizontal double-headed arrow,
 * symbolizing zooming or locking a document's scale to the viewport width.
 * </p>
 *
 * @author anahata
 */
public class FitWidthIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new FitWidthIcon of the specified square size.
     *
     * @param size The size of the icon in pixels.
     */
    public FitWidthIcon(int size) {
        super(size);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Paints a document frame containing a horizontal double-headed arrow.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            int w = size - 2;
            int h = size - 2;
            int ox = x + 1;
            int oy = y + 1;

            Color frameColor = UIManager.getColor("Button.foreground");
            if (frameColor == null) {
                frameColor = (c != null) ? c.getForeground() : Color.LIGHT_GRAY;
            }

            // Outer document boundary
            g2.setColor(c != null && !c.isEnabled() ? Color.GRAY : frameColor);
            g2.setStroke(new BasicStroke(1.1f));
            g2.drawRoundRect(ox, oy, w, h, 2, 2);

            // Double horizontal arrow <--->
            int midY = oy + h / 2;
            int leftX = ox + 3;
            int rightX = ox + w - 3;

            Color arrowColor = getBlueColor(c);
            g2.setColor(c != null && !c.isEnabled() ? Color.DARK_GRAY : arrowColor);
            g2.setStroke(new BasicStroke(1.4f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            g2.drawLine(leftX, midY, rightX, midY);

            // Left arrowhead <
            g2.drawLine(leftX, midY, leftX + 2, midY - 2);
            g2.drawLine(leftX, midY, leftX + 2, midY + 2);

            // Right arrowhead >
            g2.drawLine(rightX, midY, rightX - 2, midY - 2);
            g2.drawLine(rightX, midY, rightX - 2, midY + 2);
        } finally {
            g2.dispose();
        }
    }
}
