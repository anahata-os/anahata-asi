package uno.anahata.ai.swing;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;
import javax.swing.Icon;

/**
 * A simple, programmatically drawn Icon that shows a microphone.
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
        g2d.setColor(c.isEnabled() ? Color.BLACK : Color.GRAY);

        // All calculations are based on the icon size
        int micHeadDiameter = size / 2;
        int micHeadX = x + (size - micHeadDiameter) / 2;
        int micHeadY = y + 2;

        int micBodyWidth = size / 4;
        int micBodyHeight = size / 4;
        int micBodyX = x + (size - micBodyWidth) / 2;
        int micBodyY = micHeadY + micHeadDiameter - 2;

        int standWidth = size / 2;
        int standHeight = 2;
        int standX = x + (size - standWidth) / 2;
        int standY = micBodyY + micBodyHeight;

        // Draw Microphone Head (Oval)
        g2d.draw(new Ellipse2D.Double(micHeadX, micHeadY, micHeadDiameter, micHeadDiameter));

        // Draw Microphone Body (Rectangle)
        g2d.fill(new Rectangle2D.Double(micBodyX, micBodyY, micBodyWidth, micBodyHeight));
        
        // Draw Stand (Line/Rectangle)
        g2d.fill(new Rectangle2D.Double(standX, standY, standWidth, standHeight));

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
