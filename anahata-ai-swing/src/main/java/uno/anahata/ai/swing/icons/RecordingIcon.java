package uno.anahata.ai.swing.icons;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import javax.swing.Icon;

/**
 * A simple, programmatically drawn Icon that shows a filled red circle
 * to indicate that a recording is in progress.
 */
public class RecordingIcon implements Icon {

    private final int size;

    public RecordingIcon(int size) {
        this.size = size;
    }

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

    @Override
    public int getIconWidth() {
        return size;
    }

    @Override
    public int getIconHeight() {
        return size;
    }
}
