/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Path2D;

/**
 * A programmatically drawn Fast-Forward (double right arrow/chevron) icon.
 * <p>
 * Stylized with adaptive Barça Blue, representing jumping to the end
 * of a document (last page) or fast-forwarding playback.
 * </p>
 *
 * @author anahata
 */
public class FastForwardIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new FastForwardIcon with the specified size.
     *
     * @param size The size in pixels.
     */
    public FastForwardIcon(int size) {
        super(size);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Renders two tall, anti-aliased right chevrons side-by-side.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        try {
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

            Color blue = getBlueColor(c);
            g2d.setColor(blue);

            float thickness = Math.max(1.5f, size / 7f);
            g2d.setStroke(new BasicStroke(thickness, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));

            // Left chevron
            Path2D path1 = new Path2D.Double();
            path1.moveTo(x + size * 0.18, y + size * 0.16);
            path1.lineTo(x + size * 0.48, y + size * 0.5);
            path1.lineTo(x + size * 0.18, y + size * 0.84);
            g2d.draw(path1);

            // Right chevron
            Path2D path2 = new Path2D.Double();
            path2.moveTo(x + size * 0.52, y + size * 0.16);
            path2.lineTo(x + size * 0.82, y + size * 0.5);
            path2.lineTo(x + size * 0.52, y + size * 0.84);
            g2d.draw(path2);
        } finally {
            g2d.dispose();
        }
    }
}
