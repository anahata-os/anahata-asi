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
import java.awt.geom.Path2D;
import javax.swing.Icon;

/**
 * A programmatically drawn Icon representing the "Load Session" action.
 * It is stylized as a folder with an arrow pointing out, using the full Barça palette.
 *
 * @author pablo
 */
public class LoadSessionIcon implements Icon {

    private final int size;

    public LoadSessionIcon(int size) {
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

        Color barcaRed = new Color(165, 0, 68);
        Color barcaBlue = new Color(0, 77, 152);
        Color barcaYellow = new Color(255, 205, 0);

        g2d.setStroke(new BasicStroke(size * 0.08f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));

        double folderWidth = size * 0.75;
        double folderHeight = size * 0.6;
        double folderX = x + (size - folderWidth) / 2.0;
        double folderY = y + (size - folderHeight) / 2.0 + 2;
        
        // Folder shape
        Path2D folder = new Path2D.Double();
        folder.moveTo(folderX, folderY);
        folder.lineTo(folderX + folderWidth * 0.35, folderY);
        folder.lineTo(folderX + folderWidth * 0.45, folderY + folderHeight * 0.15);
        folder.lineTo(folderX + folderWidth, folderY + folderHeight * 0.15);
        folder.lineTo(folderX + folderWidth, folderY + folderHeight);
        folder.lineTo(folderX, folderY + folderHeight);
        folder.closePath();

        if (c.isEnabled()) {
            // Fill with Yellow
            g2d.setColor(barcaYellow);
            g2d.fill(folder);
            // Border with Blue
            g2d.setColor(barcaBlue);
            g2d.draw(folder);
            
            // Arrow pointing out (Red)
            g2d.setColor(barcaRed);
            double arrowX = folderX + folderWidth * 0.3;
            double arrowY = folderY + folderHeight * 0.6;
            double arrowLength = folderWidth * 0.5;
            
            g2d.drawLine((int)arrowX, (int)arrowY, (int)(arrowX + arrowLength), (int)(arrowY - arrowLength * 0.6));
            
            Path2D head = new Path2D.Double();
            head.moveTo(arrowX + arrowLength - 4, arrowY - arrowLength * 0.6 - 4);
            head.lineTo(arrowX + arrowLength, arrowY - arrowLength * 0.6);
            head.lineTo(arrowX + arrowLength - 6, arrowY - arrowLength * 0.6 + 2);
            g2d.draw(head);
        } else {
            g2d.setColor(Color.GRAY);
            g2d.draw(folder);
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
