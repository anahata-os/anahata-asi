/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.*;
import java.awt.geom.*;

/**
 * A programmatically drawn Icon representing screen sharing status.
 */
public class ScreenShareIcon extends AbstractAnahataIcon {
    
    /**
     * Flag indicating if active screen sharing is occurring.
     */
    private final boolean sharing;

    /**
     * Constructs a new ScreenShareIcon.
     * @param size The square dimension of the icon.
     * @param sharing True if currently sharing, false otherwise.
     */
    public ScreenShareIcon(int size, boolean sharing) {
        super(size);
        this.sharing = sharing;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        double pad = size * 0.1;
        double sw = size - 2 * pad;
        double sh = sw * 0.7;
        double sx = x + pad;
        double sy = y + pad;

        // 1. Stand
        g2.setColor(Color.GRAY);
        double neckW = sw * 0.15;
        double neckH = size * 0.1;
        g2.fill(new Rectangle2D.Double(x + (size - neckW) / 2.0, sy + sh, neckW, neckH));
        
        double baseW = sw * 0.5;
        double baseH = size * 0.05;
        g2.fill(new RoundRectangle2D.Double(x + (size - baseW) / 2.0, sy + sh + neckH, baseW, baseH, 2, 2));

        // 2. Outer Frame
        Color frameColor = sharing ? new Color(50, 200, 120) : new Color(80, 80, 80);
        g2.setColor(frameColor);
        g2.fill(new RoundRectangle2D.Double(sx, sy, sw, sh, 4, 4));

        // 3. Screen Area
        g2.setColor(Color.BLACK);
        double border = size * 0.05;
        g2.fill(new Rectangle2D.Double(sx + border, sy + border, sw - 2 * border, sh - 2 * border));

        // 4. Status Indicator
        if (sharing) {
            // Recording dot
            g2.setColor(Color.RED);
            double dotSize = sw * 0.2;
            g2.fill(new Ellipse2D.Double(sx + sw/2.0 - dotSize/2.0, sy + sh/2.0 - dotSize/2.0, dotSize, dotSize));
            
            // Glow effect
            g2.setColor(new Color(255, 0, 0, 80));
            g2.setStroke(new BasicStroke(2f));
            g2.draw(new Ellipse2D.Double(sx + sw/2.0 - dotSize/2.0 - 2, sy + sh/2.0 - dotSize/2.0 - 2, dotSize + 4, dotSize + 4));
        } else {
            // Inactive signal (diagonal line)
            g2.setColor(new Color(150, 150, 150, 100));
            g2.setStroke(new BasicStroke(1.5f));
            g2.draw(new Line2D.Double(sx + sw*0.3, sy + sh*0.3, sx + sw*0.7, sy + sh*0.7));
        }

        g2.dispose();
    }
}
