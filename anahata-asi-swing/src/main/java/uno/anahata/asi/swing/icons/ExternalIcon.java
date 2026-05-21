/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import javax.swing.UIManager;

/**
 * A programmatic "External" icon (Box with arrow) that scales and matches the current theme.
 * 
 * @author anahata
 */
public class ExternalIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new ExternalIcon of the specified square size.
     * @param size The size of the icon.
     */
    public ExternalIcon(int size) {
        super(size);
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            
            g2.setColor(UIManager.getColor("Button.focusedBorderColor"));
            if (g2.getColor() == null) {
                g2.setColor(c.getForeground());
            }

            int pad = size / 4;
            int thickness = Math.max(2, size / 10);
            g2.setStroke(new BasicStroke(thickness, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            
            // The box (L-shape part)
            int boxSize = (int)(size * 0.6);
            g2.drawPolyline(new int[]{x + pad, x + pad, x + pad + boxSize}, 
                           new int[]{y + pad + boxSize/2, y + pad + boxSize, y + pad + boxSize}, 3);
            g2.drawPolyline(new int[]{x + pad + boxSize, x + pad + boxSize, x + pad + boxSize/2},
                           new int[]{y + pad + boxSize/2, y + pad, y + pad}, 3);

            // The arrow
            int arrowX = x + size - pad;
            int arrowY = y + pad;
            g2.drawLine(x + size/2, y + size/2, arrowX, arrowY); // stem
            g2.drawLine(arrowX - size/4, arrowY, arrowX, arrowY); // head 1
            g2.drawLine(arrowX, arrowY, arrowX, arrowY + size/4); // head 2
            
        } finally {
            g2.dispose();
        }
    }
}
