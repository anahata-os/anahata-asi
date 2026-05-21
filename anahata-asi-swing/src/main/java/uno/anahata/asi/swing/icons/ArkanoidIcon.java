/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

/**
 * A programmatically drawn Icon representing the Arkanoid game.
 * <p>
 * Renders a classic Breakout scene: a paddle, a ball, and a target brick.
 * </p>
 * 
 * @author anahata
 */
public class ArkanoidIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new ArkanoidIcon of the specified square size.
     * @param size The size of the icon.
     */
    public ArkanoidIcon(int size) {
        super(size);
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.translate(x, y);

        double s = size;
        
        // Brick
        g2.setColor(new Color(220, 53, 69)); // Barsa Red
        g2.fillRect((int)(s * 0.1), (int)(s * 0.1), (int)(s * 0.8), (int)(s * 0.25));
        
        // Paddle
        g2.setColor(new Color(0, 123, 255)); // Barsa Blue
        g2.fillRoundRect((int)(s * 0.2), (int)(s * 0.75), (int)(s * 0.6), (int)(s * 0.15), (int)(s * 0.1), (int)(s * 0.1));
        
        // Ball
        g2.setColor(Color.YELLOW);
        g2.fillOval((int)(s * 0.45), (int)(s * 0.5), (int)(s * 0.15), (int)(s * 0.15));

        g2.dispose();
    }
}
