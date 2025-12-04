package uno.anahata.ai.swing;

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
        g2d.setColor(Color.RED);
        
        // Center the circle within the icon's area
        int diameter = size - 4; // A little padding
        int circleX = x + (size - diameter) / 2;
        int circleY = y + (size - diameter) / 2;
        
        g2d.fillOval(circleX, circleY, diameter, diameter);
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
