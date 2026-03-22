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
 * A programmatically drawn Icon representing a "Run All and Send" action.
 * <p>
 * Stylized as two overlapping play buttons (triangles) using Anahata brand colors, 
 * representing the sequential execution and submission of tool results.
 * </p>
 *
 * @author anahata
 */
public class RunAndSendIcon extends AbstractAnahataIcon {


    /**
     * Constructs a new RunAndSendIcon with the specified size.
     * @param size The size in pixels.
     */
    public RunAndSendIcon(int size) {
        super(size);
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Renders dual-play triangles symbolizing batch execution.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        
        Color anahataBlue = new Color(0, 123, 255);
        g2d.setColor(c.isEnabled() ? anahataBlue : Color.GRAY);

        int tw = size / 3;
        int th = size / 2;
        int ty = y + (size - th) / 2;
        int tx = x + (size - (tw * 2)) / 2;
        
        // First triangle
        int[] x1 = {tx, tx + tw, tx};
        int[] y1 = {ty, ty + th/2, ty + th};
        g2d.fillPolygon(x1, y1, 3);
        
        // Second triangle
        int[] x2 = {tx + tw - 2, tx + tw * 2 - 2, tx + tw - 2};
        g2d.fillPolygon(x2, y1, 3);
        
        g2d.dispose();
    }

}
