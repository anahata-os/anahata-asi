/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.*;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing the Tetris game.
 * <p>
 * Renders a stylized T-tetromino made of four shining Atoms, 
 * signifying order through modular construction.
 * </p>
 * 
 * @author anahata
 */
public class TetrisIcon extends AbstractAnahataIcon {

    public TetrisIcon(int size) {
        super(size);
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.translate(x, y);

        double s = size;
        double a = s / 3.5; // Atom size

        // T-shape colors (Purple/TShape)
        Color color = new Color(111, 66, 193);
        
        // Draw 4 atoms in a T-shape
        drawAtom(g2, s / 2 - a / 2, s / 2 - a / 2, a, color); // Center
        drawAtom(g2, s / 2 - a / 2, s / 2 - a * 1.6, a, color); // Top
        drawAtom(g2, s / 2 - a * 1.6, s / 2 - a / 2, a, color); // Left
        drawAtom(g2, s / 2 + a * 0.6, s / 2 - a / 2, a, color); // Right

        g2.dispose();
    }

    private void drawAtom(Graphics2D g2, double x, double y, double size, Color color) {
        g2.setColor(color);
        g2.fillOval((int)x, (int)y, (int)size, (int)size);
        g2.setColor(color.brighter());
        g2.setStroke(new BasicStroke(1.5f));
        g2.drawOval((int)x, (int)y, (int)size, (int)size);
        // Little shine
        g2.setColor(new Color(255, 255, 255, 120));
        g2.fillOval((int)(x + size * 0.2), (int)(y + size * 0.2), (int)(size * 0.2), (int)(size * 0.2));
    }
}
