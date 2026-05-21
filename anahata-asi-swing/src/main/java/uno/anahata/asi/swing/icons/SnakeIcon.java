/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Path2D;

/**
 * A programmatically drawn Icon representing the Snake game.
 * <p>
 * Renders a stylized, winding green snake with a small eye, 
 * signifying the agile and hungry nature of the Mapacho snake.
 * </p>
 * 
 * @author anahata
 */
public class SnakeIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new SnakeIcon of the specified square size.
     * @param size The size of the icon.
     */
    public SnakeIcon(int size) {
        super(size);
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.translate(x, y);

        g2.setColor(new Color(40, 167, 69)); // Mapacho Green
        Path2D.Double path = new Path2D.Double();
        double s = size;
        path.moveTo(s * 0.2, s * 0.8);
        path.curveTo(s * 0.4, s * 0.1, s * 0.6, s * 0.9, s * 0.8, s * 0.2);
        
        g2.setStroke(new java.awt.BasicStroke((float)(s * 0.15), java.awt.BasicStroke.CAP_ROUND, java.awt.BasicStroke.JOIN_ROUND));
        g2.draw(path);

        // Snake Eye
        g2.setColor(Color.WHITE);
        g2.fillOval((int)(s * 0.72), (int)(s * 0.2), (int)(s * 0.1), (int)(s * 0.1));
        
        g2.dispose();
    }
}
