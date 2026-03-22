/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

/**
 * A programmatically drawn Icon representing a single "Run" action.
 * <p>
 * Stylized as a single play button (triangle) using Anahata brand colors, 
 * symbolizing the execution of a single agentic tool.
 * </p>
 *
 * @author anahata
 */
public class RunIcon extends AbstractAnahataIcon {


    /**
     * Constructs a new RunIcon with the specified size.
     * @param size The size in pixels.
     */
    public RunIcon(int size) {
        super(size);
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Renders a crisp play triangle representing tool execution.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        
        Color anahataBlue = new Color(0, 123, 255);
        g2d.setColor(c.isEnabled() ? anahataBlue : Color.GRAY);

        int tw = size / 2;
        int th = size / 2;
        int tx = x + (size - tw) / 2;
        int ty = y + (size - th) / 2;
        
        int[] xPoints = {tx, tx + tw, tx};
        int[] yPoints = {ty, ty + th/2, ty + th};
        g2d.fillPolygon(xPoints, yPoints, 3);
        
        g2d.dispose();
    }

}
