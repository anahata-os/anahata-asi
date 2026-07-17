/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.RoundRectangle2D;

/**
 * A programmatically drawn Icon representing a "Link" or "URL" action.
 * Stylized with interlocked Barça-themed rings (one Garnet, one Blue).
 *
 * @author anahata
 */
public class LinkIcon extends AbstractAnahataIcon {



    /**
     * Constructs a new LinkIcon of the specified square size.
     * @param size The size of the icon.
     */
    public LinkIcon(int size) {
        super(size);
    }

    /**
     * {@inheritDoc}
     * <p>Paints two interlocked rounded rectangle rings representing a link, colored using Barça Blue and Barça Red adaptively.</p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);
            g2.translate(x, y);

            float thickness = size * 0.12f;
            g2.setStroke(new BasicStroke(thickness, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));

            double linkWidth = size * 0.55;
            double linkHeight = size * 0.3;
            double arc = linkHeight;

            // Center rotation for the "link" angle
            g2.rotate(Math.toRadians(-45), size / 2.0, size / 2.0);
            
            // Blue Link (Top-Leftish)
            g2.setColor(getBlueColor(c));
            g2.draw(new RoundRectangle2D.Double(size * 0.05, size * 0.35, linkWidth, linkHeight, arc, arc));
            
            // Red Link (Bottom-Rightish)
            g2.setColor(getRedColor(c));
            g2.draw(new RoundRectangle2D.Double(size * 0.4, size * 0.35, linkWidth, linkHeight, arc, arc));

        } finally {
            g2.dispose();
        }
    }
}
