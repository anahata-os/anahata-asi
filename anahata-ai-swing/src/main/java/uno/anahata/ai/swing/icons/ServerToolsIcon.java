/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing server-side tool execution.
 * Stylized as a dark rack server with multi-colored status LEDs.
 *
 * @author pablo
 */
public class ServerToolsIcon implements Icon {

    private final int size;

    public ServerToolsIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

        Color brightRed = new Color(255, 50, 50);
        Color brightBlue = new Color(50, 150, 255);
        Color anahataGreen = new Color(40, 167, 69);
        Color barcaYellow = new Color(255, 205, 0);
        Color borderBlue = new Color(0, 77, 152);

        if (c.isEnabled()) {
            double margin = size * 0.05;
            double rackX = x + margin;
            double rackY = y + margin;
            double rackW = size - 2 * margin;
            double rackH = size - 2 * margin;

            // Rack frame
            g2d.setColor(Color.BLACK);
            g2d.fill(new Rectangle2D.Double(rackX, rackY, rackW, rackH));
            g2d.setColor(borderBlue);
            g2d.setStroke(new BasicStroke((float) (size * 0.04)));
            g2d.draw(new Rectangle2D.Double(rackX, rackY, rackW, rackH));

            double unitH = rackH / 3.0;
            double ledSize = size * 0.09;
            double ledSpacing = size * 0.13;
            double ledX1 = rackX + rackW * 0.15;
            double ledX2 = ledX1 + ledSpacing;

            for (int i = 0; i < 3; i++) {
                double uy = rackY + i * unitH;
                g2d.setColor(new Color(45, 45, 45));
                g2d.fill(new Rectangle2D.Double(rackX + rackW * 0.05, uy + unitH * 0.1, rackW * 0.9, unitH * 0.8));

                double ledY = uy + (unitH - ledSize) / 2.0;

                if (i == 0) {
                    // Top: Green + Red
                    g2d.setColor(anahataGreen);
                    g2d.fill(new Ellipse2D.Double(ledX1, ledY, ledSize, ledSize));
                    g2d.setColor(brightRed);
                    g2d.fill(new Ellipse2D.Double(ledX2, ledY, ledSize, ledSize));
                } else if (i == 1) {
                    // Middle: Green + Yellow
                    g2d.setColor(anahataGreen);
                    g2d.fill(new Ellipse2D.Double(ledX1, ledY, ledSize, ledSize));
                    g2d.setColor(barcaYellow);
                    g2d.fill(new Ellipse2D.Double(ledX2, ledY, ledSize, ledSize));
                } else {
                    // Bottom: Yellow + Blue
                    g2d.setColor(barcaYellow);
                    g2d.fill(new Ellipse2D.Double(ledX1, ledY, ledSize, ledSize));
                    g2d.setColor(brightBlue);
                    g2d.fill(new Ellipse2D.Double(ledX2, ledY, ledSize, ledSize));
                }
            }
        } else {
            g2d.setColor(Color.GRAY);
            g2d.draw(new Rectangle2D.Double(x + 2, y + 2, size - 4, size - 4));
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
