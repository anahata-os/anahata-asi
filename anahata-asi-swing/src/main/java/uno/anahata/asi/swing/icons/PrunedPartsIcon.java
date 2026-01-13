package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Path2D;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing a "pruned" or "hidden" part.
 * It is drawn as a leaf using the Anahata brand Green.
 */
public class PrunedPartsIcon implements Icon {

    private final int size;

    public PrunedPartsIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        
        Color anahataGreen = new Color(40, 167, 69);
        g2d.setColor(c.isEnabled() ? anahataGreen : Color.GRAY);
        
        Path2D leaf = new Path2D.Double();
        leaf.moveTo(x + size/2, y + size - 2);
        leaf.curveTo(x + size - 2, y + size/2, x + size/2, y + 2, x + size/2, y + 2);
        leaf.curveTo(x + 2, y + size/2, x + size/2, y + size - 2, x + size/2, y + size - 2);
        g2d.fill(leaf);
        
        g2d.setColor(Color.WHITE);
        g2d.setStroke(new BasicStroke(1.0f));
        g2d.drawLine(x + size/2, y + size - 4, x + size/2, y + 4);

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
