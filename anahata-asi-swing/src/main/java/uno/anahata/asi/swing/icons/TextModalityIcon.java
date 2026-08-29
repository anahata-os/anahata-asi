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
 * A vector icon rendering a text document page with horizontal line markings.
 * <p>
 * Primarily used to represent text response modalities and code/markdown generation capabilities
 * across the ASI user interface.
 * </p>
 * 
 * @author anahata
 */
public class TextModalityIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new TextModalityIcon with the specified square dimensions.
     * 
     * @param size The width and height in pixels.
     */
    public TextModalityIcon(int size) {
        super(size);
    }

    /**
     * {@inheritDoc}
     * <p>Paints the document page outline and horizontal text lines using anti-aliased strokes.</p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

            float s = size;
            Color primary = (c != null && c.getForeground() != null) ? c.getForeground() : getBlueColor(c);
            g2.setColor(primary);

            float stroke = Math.max(1.2f, s * 0.075f);
            g2.setStroke(new BasicStroke(stroke, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));

            // Page outline
            float pad = s * 0.12f;
            float w = s * 0.64f;
            float h = s * 0.76f;
            g2.draw(new RoundRectangle2D.Float(x + s * 0.18f, y + pad, w, h, s * 0.12f, s * 0.12f));

            // Text lines
            float lineStroke = Math.max(1.0f, s * 0.065f);
            g2.setStroke(new BasicStroke(lineStroke, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            g2.drawLine((int) (x + s * 0.30f), (int) (y + s * 0.32f), (int) (x + s * 0.70f), (int) (y + s * 0.32f));
            g2.drawLine((int) (x + s * 0.30f), (int) (y + s * 0.50f), (int) (x + s * 0.70f), (int) (y + s * 0.50f));
            g2.drawLine((int) (x + s * 0.30f), (int) (y + s * 0.68f), (int) (x + s * 0.56f), (int) (y + s * 0.68f));
        } finally {
            g2.dispose();
        }
    }
}
