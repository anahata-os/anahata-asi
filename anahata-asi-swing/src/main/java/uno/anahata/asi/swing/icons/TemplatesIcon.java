/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Line2D;
import java.awt.geom.Path2D;
import java.awt.geom.RoundRectangle2D;

/**
 * A programmatically drawn vector icon representing session templates and blueprints.
 * <p>
 * Displays a stencil document sheet with modular layout markers, ruler calibrations,
 * and a centered amber stencil diamond cutout.
 * </p>
 *
 * @author anahata
 */
public class TemplatesIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new TemplatesIcon with the specified dimension.
     *
     * @param size The square icon dimension in pixels.
     */
    public TemplatesIcon(int size) {
        super(size);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Paints a stencil blueprint sheet with notch calibrations and an amber diamond cutout.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        double pad = size * 0.15;
        double w = size - 2 * pad;
        double h = size - 2 * pad;
        double sx = x + pad;
        double sy = y + pad;

        Color baseColor = (c != null && c.getForeground() != null) ? c.getForeground() : Color.DARK_GRAY;

        // 1. Sheet outline with rounded corners
        g2.setColor(baseColor);
        g2.setStroke(new BasicStroke(1.4f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        g2.draw(new RoundRectangle2D.Double(sx, sy, w, h, 3, 3));

        // 2. Amber diamond stencil cutout
        Path2D.Double diamond = new Path2D.Double();
        double cx = sx + w / 2.0;
        double dy = sy + h * 0.32;
        double dr = w * 0.22;
        diamond.moveTo(cx, dy - dr);
        diamond.lineTo(cx + dr, dy);
        diamond.lineTo(cx, dy + dr);
        diamond.lineTo(cx - dr, dy);
        diamond.closePath();
        g2.setColor(new Color(230, 160, 40));
        g2.fill(diamond);

        // 3. Ruler notch markers on the right edge
        g2.setColor(baseColor);
        g2.setStroke(new BasicStroke(1.2f));
        g2.draw(new Line2D.Double(sx + w - 3, sy + h * 0.25, sx + w, sy + h * 0.25));
        g2.draw(new Line2D.Double(sx + w - 4, sy + h * 0.5, sx + w, sy + h * 0.5));
        g2.draw(new Line2D.Double(sx + w - 3, sy + h * 0.75, sx + w, sy + h * 0.75));

        // 4. Bottom layout slot bar
        g2.fill(new RoundRectangle2D.Double(sx + w * 0.2, sy + h * 0.7, w * 0.6, 2, 1, 1));

        g2.dispose();
    }
}
