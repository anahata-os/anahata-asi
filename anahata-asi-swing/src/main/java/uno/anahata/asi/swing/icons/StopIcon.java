/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

/**
 * A programmatically drawn stop icon (square).
 * <p>
 * Renders a solid Barça Red square to signify the termination of an active process.
 * </p>
 * 
 * @author anahata
 */
public class StopIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new StopIcon with the specified size.
     * @param size The size in pixels.
     */
    public StopIcon(int size) {
        super(size);
    }

    /** 
     * {@inheritDoc} 
     * <p>Renders a centered square using the Barça Red palette.</p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        if (c.isEnabled()) {
            g2d.setColor(new Color(165, 0, 68)); // Barça Red
            int margin = size / 4;
            g2d.fillRect(x + margin, y + margin, size - (margin * 2), size - (margin * 2));
        } else {
            g2d.setColor(Color.GRAY);
            int margin = size / 4;
            g2d.drawRect(x + margin, y + margin, size - (margin * 2), size - (margin * 2));
        }

        g2d.dispose();
    }
}
