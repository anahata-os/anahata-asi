/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Path2D;

/**
 * A programmatically drawn Icon representing the V2 Resources (Prism).
 * <p>
 * Stylized as a diamond/prism shape using the Barça Orange to symbolize the 
 * multi-faceted nature of managed resources and their refraction into context.
 * </p>
 * 
 * @author anahata
 */
public class PrismIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new PrismIcon with the specified size.
     * @param size The size in pixels.
     */
    public PrismIcon(int size) {
        super(size);
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Renders a geometric prism symbol.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setColor(new Color(0xFF9800)); // Orange
        
        Path2D p = new Path2D.Double();
        p.moveTo(x + size / 2, y + 2);
        p.lineTo(x + size - 2, y + size / 2);
        p.lineTo(x + size / 2, y + size - 2);
        p.lineTo(x + 2, y + size / 2);
        p.closePath();
        g2.fill(p);
        g2.dispose();
    }
}
