/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

/**
 * A programmatically drawn Icon representing the AgiKart game.
 * <p>
 * Renders a stylized, beautiful retro racing kart in motion, matching the
 * color identity of the Anahata ecosystem and showcasing the Mode 7 speedway aesthetics.
 * </p>
 * 
 * @author anahata
 */
public class AgiKartIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new AgiKartIcon of the specified square size.
     * @param size The square size of the icon.
     */
    public AgiKartIcon(int size) {
        super(size);
    }

    /**
     * {@inheritDoc}
     * <p>Paints a gorgeous vector-based representation of a retro racing kart
     * traversing a curb on a dark asphalt track.</p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.translate(x, y);

        double s = size;
        
        // 1. Draw a beautiful golden/yellow circular badge background
        g2.setColor(new Color(241, 196, 15, 45)); // Soft golden glow
        g2.fillOval((int)(s * 0.05), (int)(s * 0.05), (int)(s * 0.9), (int)(s * 0.9));
        g2.setColor(new Color(241, 196, 15, 180)); // Golden border
        g2.drawOval((int)(s * 0.05), (int)(s * 0.05), (int)(s * 0.9), (int)(s * 0.9));

        // 2. Road surface
        g2.setColor(new Color(60, 60, 60));
        g2.fillOval((int)(s * 0.1), (int)(s * 0.5), (int)(s * 0.8), (int)(s * 0.4));
        g2.setColor(new Color(80, 80, 80));
        g2.fillRect((int)(s * 0.1), (int)(s * 0.65), (int)(s * 0.8), (int)(s * 0.25));
        
        // 3. Curb (alternating red/white blocks)
        g2.setColor(new Color(220, 53, 69)); // Barça Red
        g2.fillRect((int)(s * 0.1), (int)(s * 0.6), (int)(s * 0.4), (int)(s * 0.06));
        g2.setColor(Color.WHITE);
        g2.fillRect((int)(s * 0.5), (int)(s * 0.6), (int)(s * 0.4), (int)(s * 0.06));

        // 4. Shadow
        g2.setColor(new Color(0, 0, 0, 130));
        g2.fillOval((int)(s * 0.2), (int)(s * 0.72), (int)(s * 0.6), (int)(s * 0.15));

        // 5. Exhaust Flame (glowing orange/yellow)
        g2.setColor(Color.ORANGE);
        g2.fillOval((int)(s * 0.1), (int)(s * 0.62), (int)(s * 0.12), (int)(s * 0.12));
        g2.setColor(Color.YELLOW);
        g2.fillOval((int)(s * 0.12), (int)(s * 0.64), (int)(s * 0.08), (int)(s * 0.08));

        // 6. Rear Wheels
        g2.setColor(Color.DARK_GRAY);
        g2.fillRect((int)(s * 0.22), (int)(s * 0.62), (int)(s * 0.12), (int)(s * 0.16));
        g2.fillRect((int)(s * 0.66), (int)(s * 0.62), (int)(s * 0.12), (int)(s * 0.16));

        // 7. Kart Frame (Barça Blue)
        g2.setColor(new Color(0, 123, 255));
        g2.fillRect((int)(s * 0.32), (int)(s * 0.48), (int)(s * 0.36), (int)(s * 0.22));
        g2.setColor(Color.BLACK);
        g2.drawRect((int)(s * 0.32), (int)(s * 0.48), (int)(s * 0.36), (int)(s * 0.22));

        // 8. Driver Helmet (Barça Red)
        g2.setColor(new Color(220, 53, 69));
        g2.fillOval((int)(s * 0.38), (int)(s * 0.26), (int)(s * 0.24), (int)(s * 0.24));
        g2.setColor(Color.BLACK);
        g2.drawOval((int)(s * 0.38), (int)(s * 0.26), (int)(s * 0.24), (int)(s * 0.24));

        // Visor
        g2.setColor(Color.BLACK);
        g2.fillRect((int)(s * 0.42), (int)(s * 0.31), (int)(s * 0.16), (int)(s * 0.07));

        // 9. Retro "AGI" Title Overlay (when icon size is reasonably big)
        if (s >= 32) {
            g2.setFont(new Font("Impact", Font.ITALIC, (int) (s * 0.18)));
            String text = "AGI";
            FontMetrics fm = g2.getFontMetrics();
            int tx = (int) (s - fm.stringWidth(text)) / 2;
            int ty = (int) (s * 0.22);
            
            // Text shadow
            g2.setColor(Color.BLACK);
            g2.drawString(text, tx + 1, ty + 1);
            
            // Text gold/yellow gradient color
            g2.setColor(new Color(255, 215, 0));
            g2.drawString(text, tx, ty);
        }

        g2.dispose();
    }
}
