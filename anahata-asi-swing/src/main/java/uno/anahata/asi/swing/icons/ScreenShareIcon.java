/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import lombok.Getter;

/**
 * A programmatically drawn Icon representing screen sharing status.
 * <p>
 * Displays a monitor frame with an idle diagonal line when no screens or regions
 * are shared, or an emerald green frame with a centered count when active sharing
 * is in progress.
 * </p>
 *
 * @author anahata
 */
@Getter
public class ScreenShareIcon extends AbstractAnahataIcon {

    /**
     * Flag indicating if active screen sharing is occurring.
     */
    private final boolean sharing;

    /**
     * The total count of shared displays and regions.
     */
    private final int count;

    /**
     * Constructs a new ScreenShareIcon with sharing inactive.
     *
     * @param size The square dimension of the icon.
     */
    public ScreenShareIcon(int size) {
        this(size, 0);
    }

    /**
     * Constructs a new ScreenShareIcon with boolean sharing status.
     *
     * @param size The square dimension of the icon.
     * @param sharing True if currently sharing, false otherwise.
     */
    public ScreenShareIcon(int size, boolean sharing) {
        this(size, sharing ? 1 : 0);
    }

    /**
     * Constructs a new ScreenShareIcon with a specific count of shared items.
     *
     * @param size The square dimension of the icon.
     * @param count The total number of items being shared.
     */
    public ScreenShareIcon(int size, int count) {
        super(size);
        this.count = Math.max(0, count);
        this.sharing = this.count > 0;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Paints a monitor screen share icon with its frame colored adaptively, showing
     * a centered item count inside the screen area when active screen sharing is occurring.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

        double pad = size * 0.1;
        double sw = size - 2 * pad;
        double sh = sw * 0.7;
        double sx = x + pad;
        double sy = y + pad;

        // 1. Stand
        g2.setColor(getRedColor(c));
        double neckW = sw * 0.15;
        double neckH = size * 0.1;
        g2.fill(new Rectangle2D.Double(x + (size - neckW) / 2.0, sy + sh, neckW, neckH));

        double baseW = sw * 0.5;
        double baseH = size * 0.05;
        g2.fill(new RoundRectangle2D.Double(x + (size - baseW) / 2.0, sy + sh + neckH, baseW, baseH, 2, 2));

        // 2. Outer Frame
        Color frameColor = sharing ? new Color(50, 200, 120) : getBlueColor(c);
        g2.setColor(frameColor);
        g2.fill(new RoundRectangle2D.Double(sx, sy, sw, sh, 4, 4));

        // 3. Screen Area
        g2.setColor(Color.BLACK);
        double border = size * 0.05;
        double screenX = sx + border;
        double screenY = sy + border;
        double screenW = sw - 2 * border;
        double screenH = sh - 2 * border;
        g2.fill(new Rectangle2D.Double(screenX, screenY, screenW, screenH));

        // 4. Status Indicator (Count number or inactive line)
        if (count > 0) {
            String text = String.valueOf(count);
            int fontSize = Math.max(9, (int) Math.round(screenH * 0.85));
            Font font = new Font("SansSerif", Font.BOLD, fontSize);
            g2.setFont(font);
            FontMetrics fm = g2.getFontMetrics();
            int textW = fm.stringWidth(text);
            int textH = fm.getAscent();
            float textX = (float) (screenX + (screenW - textW) / 2.0);
            float textY = (float) (screenY + (screenH + textH) / 2.0 - 1);

            g2.setColor(new Color(50, 220, 130)); // Bright emerald green
            g2.drawString(text, textX, textY);
        } else {
            // Inactive signal (diagonal line)
            g2.setColor(new Color(150, 150, 150, 100));
            g2.setStroke(new BasicStroke(1.5f));
            g2.draw(new Line2D.Double(sx + sw * 0.3, sy + sh * 0.3, sx + sw * 0.7, sy + sh * 0.7));
        }

        g2.dispose();
    }
}
