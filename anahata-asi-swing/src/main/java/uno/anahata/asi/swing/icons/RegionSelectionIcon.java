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
 * A specialized icon representing a rectangular region selection with a mouse pointer.
 * <p>
 * This icon is used in the Screen Sharing management interfaces to visually 
 * indicate the "Define Region" action.
 * </p>
 * 
 * @author anahata
 */
public class RegionSelectionIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new RegionSelectionIcon with the specified size.
     * 
     * @param size The square dimension of the icon.
     */
    public RegionSelectionIcon(int size) {
        super(size);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Renders a dashed rectangle representing a selection area and a red 
     * mouse cursor overlaying the corner.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.translate(x, y);

        float s = getSize();
        
        // 1. The Region Rectangle (Dashed/Border)
        g2.setColor(c.getForeground());
        g2.setStroke(new BasicStroke(1.2f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, new float[]{3.0f}, 0.0f));
        g2.drawRoundRect((int)(s*0.05), (int)(s*0.1), (int)(s*0.8), (int)(s*0.7), 2, 2);

        // 2. The Mouse Pointer (Red cursor)
        g2.translate(s * 0.6, s * 0.55);
        g2.setColor(getRedColor(c));
        
        Path2D.Double pointer = new Path2D.Double();
        pointer.moveTo(0, 0);
        pointer.lineTo(s * 0.35, s * 0.2);
        pointer.lineTo(s * 0.18, s * 0.2);
        pointer.lineTo(s * 0.28, s * 0.45);
        pointer.lineTo(s * 0.18, s * 0.5);
        pointer.lineTo(s * 0.08, s * 0.25);
        pointer.lineTo(0, s * 0.35);
        pointer.closePath();
        
        g2.setStroke(new BasicStroke(1.0f));
        g2.fill(pointer);
        g2.setColor(Color.WHITE);
        g2.draw(pointer);

        g2.dispose();
    }
}
