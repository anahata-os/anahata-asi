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
import uno.anahata.asi.swing.agi.SwingAgiConfig;

/**
 * A vector icon rendering an acoustic speaker with radiating sound waves.
 * <p>
 * Primarily used to represent audio response modalities and sound capabilities
 * across the ASI user interface.
 * </p>
 * 
 * @author anahata
 */
public class SpeakerIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new SpeakerIcon with the specified square dimensions.
     * 
     * @param size The width and height in pixels.
     */
    public SpeakerIcon(int size) {
        super(size);
    }

    /**
     * {@inheritDoc}
     * <p>Paints the speaker cone body and smooth sound waves using anti-aliased vector curves.</p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

            float s = size;
            Color primary = (c != null && !c.isEnabled()) ? Color.GRAY
                    : (SwingAgiConfig.isDarkLaf() ? new Color(251, 191, 36) : new Color(217, 119, 6));
            g2.setColor(primary);

            // Speaker cone body
            GeneralPath path = new GeneralPath();
            path.moveTo(x + s * 0.12f, y + s * 0.36f);
            path.lineTo(x + s * 0.30f, y + s * 0.36f);
            path.lineTo(x + s * 0.52f, y + s * 0.16f);
            path.lineTo(x + s * 0.52f, y + s * 0.84f);
            path.lineTo(x + s * 0.30f, y + s * 0.64f);
            path.lineTo(x + s * 0.12f, y + s * 0.64f);
            path.closePath();
            g2.fill(path);

            // Sound waves
            g2.setStroke(new BasicStroke(Math.max(1.2f, s * 0.08f), BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            // Inner wave
            g2.draw(new Arc2D.Float(x + s * 0.40f, y + s * 0.30f, s * 0.36f, s * 0.40f, -45, 90, Arc2D.OPEN));
            // Outer wave
            g2.draw(new Arc2D.Float(x + s * 0.45f, y + s * 0.18f, s * 0.48f, s * 0.64f, -50, 100, Arc2D.OPEN));
        } finally {
            g2.dispose();
        }
    }
}
