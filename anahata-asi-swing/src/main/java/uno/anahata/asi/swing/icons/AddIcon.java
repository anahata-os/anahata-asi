/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import javax.swing.UIManager;

/**
 * A programmatic "Add" icon (+) that scales and matches the current theme.
 * 
 * @author anahata
 */
public class AddIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new AddIcon of the specified square size.
     * @param size The size of the icon.
     */
    public AddIcon(int size) {
        super(size);
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);
            
            g2.setColor(UIManager.getColor("Button.focusedBorderColor"));
            if (g2.getColor() == null) {
                g2.setColor(c.getForeground());
            }

            int pad = size / 4;
            int mid = size / 2;
            int thickness = Math.max(2, size / 8);
            
            g2.setStroke(new BasicStroke(thickness, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            
            // Horizontal line
            g2.drawLine(x + pad, y + mid, x + size - pad, y + mid);
            // Vertical line
            g2.drawLine(x + mid, y + pad, x + mid, y + size - pad);
            
        } finally {
            g2.dispose();
        }
    }
}
