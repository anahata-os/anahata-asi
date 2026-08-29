/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Arc2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.RoundRectangle2D;
import uno.anahata.asi.swing.agi.SwingAgiConfig;

/**
 * A vector icon rendering an image landscape frame with mountain peaks and sun.
 * <p>
 * Primarily used to represent image response modalities and visual generation capabilities
 * across the ASI user interface.
 * </p>
 * 
 * @author anahata
 */
public class ImageModalityIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new ImageModalityIcon with the specified square dimensions.
     * 
     * @param size The width and height in pixels.
     */
    public ImageModalityIcon(int size) {
        super(size);
    }

    /**
     * {@inheritDoc}
     * <p>Paints the picture frame, sun, and mountain landscape using anti-aliased vector paths.</p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

            float s = size;
            Color primary = (c != null && !c.isEnabled()) ? Color.GRAY
                    : (SwingAgiConfig.isDarkLaf() ? new Color(52, 211, 153) : new Color(16, 185, 129));
            g2.setColor(primary);

            float stroke = Math.max(1.2f, s * 0.075f);
            g2.setStroke(new BasicStroke(stroke, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));

            // Outer Frame
            float pad = s * 0.10f;
            float w = s - pad * 2;
            float h = s - pad * 2;
            g2.draw(new RoundRectangle2D.Float(x + pad, y + pad, w, h, s * 0.20f, s * 0.20f));

            // Sun / Circle
            float sunR = s * 0.12f;
            g2.fill(new Arc2D.Float(x + s * 0.65f, y + s * 0.25f, sunR * 2, sunR * 2, 0, 360, Arc2D.CHORD));

            // Mountain peaks
            GeneralPath m1 = new GeneralPath();
            m1.moveTo(x + pad + stroke, y + s * 0.72f);
            m1.lineTo(x + s * 0.38f, y + s * 0.44f);
            m1.lineTo(x + s * 0.60f, y + s * 0.68f);
            m1.lineTo(x + s * 0.74f, y + s * 0.52f);
            m1.lineTo(x + pad + w - stroke, y + s * 0.72f);
            g2.draw(m1);
        } finally {
            g2.dispose();
        }
    }
}
