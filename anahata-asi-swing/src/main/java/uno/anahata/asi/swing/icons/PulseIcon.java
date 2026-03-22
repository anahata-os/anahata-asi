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
 * A programmatically drawn Icon representing History (Pulse).
 * <p>
 * Stylized as an EKG/Pulse line using Barça Blue to represent the vital 
 * and continuous flow of the conversation history.
 * </p>
 * 
 * @author anahata
 */
public class PulseIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new PulseIcon with the specified size.
     * @param size The size in pixels.
     */
    public PulseIcon(int size) {
        super(size);
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Renders a rhythmic pulse wave across the icon area.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setColor(new Color(0, 77, 152)); // Blue
        g2.setStroke(new BasicStroke(1.5f));
        
        Path2D p = new Path2D.Double();
        p.moveTo(x + 2, y + size - 6);
        p.lineTo(x + 5, y + size - 6);
        p.lineTo(x + 7, y + 4);
        p.lineTo(x + 9, y + size - 2);
        p.lineTo(x + 11, y + size - 6);
        p.lineTo(x + 14, y + size - 6);
        
        g2.draw(p);
        g2.dispose();
    }
}
