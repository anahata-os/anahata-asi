/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

/**
 * A programmatically drawn Icon that shows a studio microphone.
 * <p>
 * Stylized with a Red head and Blue stand, representing audio input 
 * capabilities within the multimodal ASI interface.
 * </p>
 * 
 * @author anahata
 */
public class MicrophoneIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new MicrophoneIcon with the specified size.
     * @param size The size in pixels.
     */
    public MicrophoneIcon(int size) {
        super(size);
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Renders a high-fidelity microphone with a weighted circular base.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        
        Color barcaRed = new Color(165, 0, 68);
        Color barcaBlue = new Color(0, 77, 152);

        if (c.isEnabled()) {
            // Head (Red)
            g2d.setColor(barcaRed);
            g2d.fillRoundRect(x + size/2 - size/6, y + 4, size/3, size/2, size/3, size/3);
            
            // Stand (Blue)
            g2d.setColor(barcaBlue);
            g2d.setStroke(new BasicStroke(2.0f));
            g2d.drawArc(x + size/2 - size/4, y + size/4, size/2, size/3, 180, 180);
            g2d.drawLine(x + size/2, y + size/2 + size/12, x + size/2, y + size - 6);
            
            // Foot (Red)
            g2d.setColor(barcaRed);
            g2d.fillOval(x + size/2 - size/4, y + size - 8, size/2, 4);
        } else {
            g2d.setColor(Color.GRAY);
            g2d.drawRoundRect(x + size/2 - size/6, y + 4, size/3, size/2, size/3, size/3);
        }

        g2d.dispose();
    }
}
