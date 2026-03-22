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
 * A simple programmatic icon representing a data table.
 * <p>
 * Stylized with a header and grid lines using the component's foreground color.
 * </p>
 * 
 * @author anahata
 */
public class TableIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new TableIcon with the specified size.
     * @param size The size in pixels.
     */
    public TableIcon(int size) {
        super(size);
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Renders a wireframe table with a distinct header row to visualize data structures.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setColor(c != null ? c.getForeground() : Color.BLACK);
        
        int padding = 2;
        g2.drawRect(x + padding, y + padding, size - padding * 2, size - padding * 2);
        
        // Horizontal line (header)
        g2.drawLine(x + padding, y + padding + size / 3, x + size - padding, y + padding + size / 3);
        
        // Vertical lines
        g2.drawLine(x + padding + size / 3, y + padding, x + padding + size / 3, y + size - padding);
        g2.drawLine(x + padding + 2 * size / 3, y + padding, x + padding + 2 * size / 3, y + size - padding);
        
        g2.dispose();
    }

}
