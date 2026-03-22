/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

/**
 * A programmatically drawn Icon that indicates active recording.
 * <p>
 * Stylized as a glowing Red circle, providing urgent visual feedback during 
 * audio acquisition turns.
 * </p>
 * 
 * @author anahata
 */
public class RecordingIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new RecordingIcon with the specified size.
     * @param size The size in pixels.
     */
    public RecordingIcon(int size) {
        super(size);
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Renders a multi-layered glowing circle to simulate hardware LED behavior.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        
        // Outer glow
        g2d.setColor(new Color(255, 0, 0, 40));
        g2d.fillOval(x + 1, y + 1, size - 2, size - 2);
        
        // Main circle
        g2d.setColor(new Color(200, 0, 0));
        g2d.fillOval(x + 4, y + 4, size - 8, size - 8);
        
        // Highlight
        g2d.setColor(new Color(255, 255, 255, 120));
        g2d.fillOval(x + 7, y + 7, size / 4, size / 4);
        
        g2d.dispose();
    }
}
