/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details.
 */
package uno.anahata.ai.swing;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Path2D;
import java.awt.geom.Rectangle2D;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing the "Save Session" action.
 * It is stylized as a classic floppy disk.
 *
 * @author pablo
 */
public class SaveSessionIcon implements Icon {

    private final int size;

    public SaveSessionIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

        Color baseColor = new Color(0, 123, 255); // Blue
        g2d.setColor(c.isEnabled() ? baseColor : Color.GRAY);

        double diskSize = size * 0.8;
        double diskX = x + (size - diskSize) / 2.0;
        double diskY = y + (size - diskSize) / 2.0;
        
        // Floppy body
        g2d.setStroke(new BasicStroke(size * 0.05f));
        g2d.draw(new Rectangle2D.Double(diskX, diskY, diskSize, diskSize));
        
        // Metal shutter
        double shutterWidth = diskSize * 0.6;
        double shutterHeight = diskSize * 0.5;
        double shutterX = diskX + (diskSize - shutterWidth) / 2.0;
        double shutterY = diskY + diskSize - shutterHeight;
        g2d.fill(new Rectangle2D.Double(shutterX, shutterY, shutterWidth, shutterHeight));
        
        // Label area
        double labelHeight = diskSize * 0.3;
        g2d.draw(new Rectangle2D.Double(diskX, diskY, diskSize, labelHeight));
        
        g2d.dispose();
    }

    @Override
    public int getIconWidth() {
        return size;
    }

    @Override
    public int getIconHeight() {
        return size;
    }
}
