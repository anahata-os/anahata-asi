/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details.
 */
package uno.anahata.ai.swing;

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
 * It is stylized as a folder with an arrow pointing out.
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

        Color baseColor = new Color(40, 167, 69); // Green
        g2d.setColor(c.isEnabled() ? baseColor : Color.GRAY);
        g2d.setStroke(new BasicStroke(size * 0.05f));

        double folderWidth = size * 0.8;
        double folderHeight = size * 0.7;
        double folderX = x + (size - folderWidth) / 2.0;
        double folderY = y + (size - folderHeight) / 2.0;
        
        // Folder shape
        Path2D folder = new Path2D.Double();
        folder.moveTo(folderX, folderY);
        folder.lineTo(folderX + folderWidth * 0.4, folderY);
        folder.lineTo(folderX + folderWidth * 0.5, folderY + folderHeight * 0.2);
        folder.lineTo(folderX + folderWidth, folderY + folderHeight * 0.2);
        folder.lineTo(folderX + folderWidth, folderY + folderHeight);
        folder.lineTo(folderX, folderY + folderHeight);
        folder.closePath();
        g2d.draw(folder);
        
        // Arrow pointing out
        double arrowX = folderX + folderWidth / 2.0;
        double arrowY = folderY + folderHeight / 2.0;
        double arrowLength = folderWidth * 0.3;
        
        g2d.drawLine((int)arrowX, (int)arrowY, (int)(arrowX + arrowLength), (int)arrowY);
        g2d.drawLine((int)(arrowX + arrowLength), (int)arrowY, (int)(arrowX + arrowLength * 0.7), (int)(arrowY - arrowLength * 0.3));
        g2d.drawLine((int)(arrowX + arrowLength), (int)arrowY, (int)(arrowX + arrowLength * 0.7), (int)(arrowY + arrowLength * 0.3));
        
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
