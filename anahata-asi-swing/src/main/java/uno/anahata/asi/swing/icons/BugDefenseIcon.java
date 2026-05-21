/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.*;

/**
 * A programmatically drawn Icon representing the Bug Swarm Defense game.
 * <p>
 * Renders a stylized Barça Blue shield protecting a code block from a 
 * descending red bug.
 * </p>
 * 
 * @author anahata
 */
public class BugDefenseIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new BugDefenseIcon of the specified square size.
     * @param size The size of the icon.
     */
    public BugDefenseIcon(int size) {
        super(size);
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        // Draw Shield (Player) - Barça Blue
        g2d.setColor(new Color(0, 77, 152));
        g2d.fillRoundRect(x + 2, y + size - 8, size - 4, 6, 4, 4);
        
        // Draw Bug (Enemy) - Red
        g2d.setColor(Color.RED);
        g2d.fillOval(x + size/2 - 4, y + 2, 8, 8);
        // Antennas
        g2d.setStroke(new BasicStroke(1f));
        g2d.drawLine(x + size/2 - 2, y + 2, x + size/2 - 4, y);
        g2d.drawLine(x + size/2 + 2, y + 2, x + size/2 + 4, y);

        g2d.dispose();
    }
}
