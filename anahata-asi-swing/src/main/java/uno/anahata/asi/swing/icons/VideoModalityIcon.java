/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.GeneralPath;
import java.awt.geom.RoundRectangle2D;

/**
 * A vector icon rendering a video camera with a lens cone.
 * <p>
 * Primarily used to represent video response modalities and video generation capabilities
 * across the ASI user interface.
 * </p>
 * 
 * @author anahata
 */
public class VideoModalityIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new VideoModalityIcon with the specified square dimensions.
     * 
     * @param size The width and height in pixels.
     */
    public VideoModalityIcon(int size) {
        super(size);
    }

    /**
     * {@inheritDoc}
     * <p>Paints the video camera body and projecting lens cone using anti-aliased geometric paths.</p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

            float s = size;
            Color primary = (c != null && c.getForeground() != null) ? c.getForeground() : getRedColor(c);
            g2.setColor(primary);

            // Main camera body
            float pad = s * 0.10f;
            float bodyW = s * 0.52f;
            float bodyH = s * 0.54f;
            g2.fill(new RoundRectangle2D.Float(x + pad, y + s * 0.23f, bodyW, bodyH, s * 0.16f, s * 0.16f));

            // Lens cone (trapezoid)
            GeneralPath lens = new GeneralPath();
            lens.moveTo(x + pad + bodyW + s * 0.04f, y + s * 0.34f);
            lens.lineTo(x + s * 0.90f, y + s * 0.22f);
            lens.lineTo(x + s * 0.90f, y + s * 0.78f);
            lens.lineTo(x + pad + bodyW + s * 0.04f, y + s * 0.66f);
            lens.closePath();
            g2.fill(lens);
        } finally {
            g2.dispose();
        }
    }
}
