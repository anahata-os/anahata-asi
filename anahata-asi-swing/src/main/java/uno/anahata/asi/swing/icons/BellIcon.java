/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Arc2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Path2D;
import java.awt.geom.RoundRectangle2D;
import lombok.Getter;

/**
 * A programmatically drawn vector icon representing audio notification status.
 * <p>
 * Displays an acoustic ringing bell with sound waves when active, or a muted bell
 * with a high-contrast diagonal red strike-through when audio feedback is disabled.
 * </p>
 *
 * @author anahata
 */
@Getter
public class BellIcon extends AbstractAnahataIcon {

    /**
     * Flag indicating whether audio feedback is muted.
     */
    private final boolean muted;

    /**
     * Constructs a new unmuted BellIcon with the specified dimension.
     *
     * @param size The square icon dimension in pixels.
     */
    public BellIcon(int size) {
        this(size, false);
    }

    /**
     * Constructs a new BellIcon with the specified dimension and mute state.
     *
     * @param size The square icon dimension in pixels.
     * @param muted True if audio notifications are muted, false otherwise.
     */
    public BellIcon(int size, boolean muted) {
        super(size);
        this.muted = muted;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Paints an acoustic bell adaptively colored to match the component's foreground,
     * drawing radiating emerald sound waves when active or a red slash when muted.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        double cx = x + (muted ? size / 2.0 : size * 0.42);
        double topY = y + size * 0.18;
        double bottomY = y + size * 0.72;
        double bellW = size * 0.55;

        Color baseColor = (c != null && c.getForeground() != null) ? c.getForeground() : Color.DARK_GRAY;

        // 1. Crown loop
        double loopR = size * 0.1;
        g2.setColor(baseColor);
        g2.setStroke(new BasicStroke(1.3f));
        g2.draw(new Ellipse2D.Double(cx - loopR, y + size * 0.1, loopR * 2, loopR * 2));

        // 2. Bell body
        Path2D.Double bell = new Path2D.Double();
        bell.moveTo(cx - size * 0.12, topY + size * 0.08);
        bell.curveTo(cx - size * 0.15, topY + size * 0.12,
                     cx - bellW * 0.45, bottomY - size * 0.1,
                     cx - bellW / 2.0, bottomY);
        bell.lineTo(cx + bellW / 2.0, bottomY);
        bell.curveTo(cx + bellW * 0.45, bottomY - size * 0.1,
                     cx + size * 0.15, topY + size * 0.12,
                     cx + size * 0.12, topY + size * 0.08);
        bell.closePath();

        g2.setColor(baseColor);
        g2.setStroke(new BasicStroke(1.4f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        g2.draw(bell);

        // 3. Rim lip
        double lipH = Math.max(1.5, size * 0.07);
        g2.fill(new RoundRectangle2D.Double(cx - bellW * 0.52, bottomY, bellW * 1.04, lipH, 1, 1));

        // 4. Clapper
        double clapperR = size * 0.09;
        g2.fill(new Arc2D.Double(cx - clapperR, bottomY + lipH - 1, clapperR * 2, clapperR * 2, 0, -180, Arc2D.CHORD));

        if (!muted) {
            // Sound waves radiating to the right
            g2.setColor(new Color(50, 180, 100)); // Emerald sound wave
            g2.setStroke(new BasicStroke(1.4f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            double waveX = cx + bellW * 0.48;
            g2.draw(new Arc2D.Double(waveX - 4, topY - 2, size * 0.35, size * 0.6, -45, 90, Arc2D.OPEN));
            if (size >= 20) {
                g2.draw(new Arc2D.Double(waveX + 1, topY - 5, size * 0.5, size * 0.75, -45, 90, Arc2D.OPEN));
            }
        } else {
            // Diagonal red slash
            g2.setColor(new Color(225, 50, 50));
            g2.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            g2.draw(new Line2D.Double(x + size * 0.15, y + size * 0.15, x + size * 0.85, y + size * 0.85));
        }

        g2.dispose();
    }
}
