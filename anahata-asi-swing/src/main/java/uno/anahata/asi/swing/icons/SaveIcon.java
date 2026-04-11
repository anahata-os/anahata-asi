/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import javax.swing.UIManager;

/**
 * A programmatic "Save" icon (Floppy disk style) that scales and matches the current theme.
 * 
 * @author anahata
 */
public class SaveIcon extends AbstractAnahataIcon {

    public SaveIcon(int size) {
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

            int pad = size / 5;
            int arc = size / 10;
            int thickness = Math.max(2, size / 10);
            
            g2.setStroke(new BasicStroke(thickness, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER));
            
            // Outer body
            g2.drawRoundRect(x + pad, y + pad, size - 2*pad, size - 2*pad, arc, arc);
            
            // Slider / Top part
            int sliderW = size / 3;
            int sliderH = size / 4;
            g2.drawRect(x + (size - sliderW)/2, y + pad, sliderW, sliderH);
            
            // Center label area
            int labelW = size / 2;
            int labelH = size / 3;
            g2.drawRect(x + (size - labelW)/2, y + size - pad - labelH, labelW, labelH);
            
        } finally {
            g2.dispose();
        }
    }
}
