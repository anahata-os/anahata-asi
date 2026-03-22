/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Path2D;

/**
 * A programmatically drawn Icon representing a restart or clear action.
 * <p>
 * Stylized as a clean circular arrow using Anahata Blue and Green brand colors, 
 * symbolizing the resetting of conversation state.
 * </p>
 *
 * @author anahata
 */
public class RestartIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new RestartIcon with the specified size.
     * @param size The size in pixels.
     */
    public RestartIcon(int size) {
        super(size);
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Renders a looping arrow with a distinct directional head.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        
        Color anahataBlue = new Color(0, 123, 255);
        Color anahataGreen = new Color(40, 167, 69);

        g2d.setColor(c.isEnabled() ? anahataBlue : Color.GRAY);
        g2d.setStroke(new BasicStroke(size/10f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        
        int r = size - 10;
        int ox = x + 5;
        int oy = y + 5;
        
        // Circular path
        g2d.drawArc(ox, oy, r, r, 60, 300);
        
        // Arrow head (Green)
        g2d.setColor(c.isEnabled() ? anahataGreen : Color.GRAY);
        Path2D arrow = new Path2D.Double();
        arrow.moveTo(x + size/2 + 1, y + 2);
        arrow.lineTo(x + size - 3, y + 6);
        arrow.lineTo(x + size/2 + 4, y + 10);
        g2d.draw(arrow);
        
        g2d.dispose();
    }
}
