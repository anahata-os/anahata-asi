package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon that shows a microphone with a red base.
 */
public class MicrophoneIcon implements Icon {

    private final int size;

    public MicrophoneIcon(int size) {
        this.size = size;
    }

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

    @Override
    public int getIconWidth() {
        return size;
    }

    @Override
    public int getIconHeight() {
        return size;
    }
}
