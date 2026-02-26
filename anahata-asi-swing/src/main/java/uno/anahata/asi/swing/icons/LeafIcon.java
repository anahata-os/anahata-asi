/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Path2D;
import javax.swing.Icon;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * A programmatically drawn icon representing a leaf in various stages of its lifecycle.
 * Used to visualize the pruning and expiration state of context parts.
 * 
 * @author anahata
 */
@RequiredArgsConstructor
public class LeafIcon implements Icon {

    /** Stage of the leaf's lifecycle. */
    public enum LeafState {
        /** Active, full of life (Green). Representing AUTO + Not Expired. */
        ACTIVE,
        /** Withered, ready to fall (Light Brown). Representing AUTO + Expired. */
        WITHERED,
        /** Dead and dry (Dark Brown). Representing explicitly PRUNED. */
        DEAD
    }

    private final int size;
    @Getter
    private final LeafState state;

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        
        Color leafColor;
        switch (state) {
            case ACTIVE -> leafColor = new Color(40, 167, 69); // Anahata Green
            case WITHERED -> leafColor = new Color(180, 130, 70); // Withered Orange/Brown
            case DEAD -> leafColor = new Color(110, 70, 30); // Darker, Dead Brown
            default -> leafColor = Color.GRAY;
        }
        
        if (!c.isEnabled()) {
            leafColor = Color.LIGHT_GRAY;
        }

        g2d.setColor(leafColor);
        
        // Draw the leaf shape
        Path2D leaf = new Path2D.Double();
        leaf.moveTo(x + size / 2.0, y + size - 2.0);
        leaf.curveTo(x + size - 2.0, y + size / 2.0, x + size / 2.0, y + 2.0, x + size / 2.0, y + 2.0);
        leaf.curveTo(x + 2.0, y + size / 2.0, x + size / 2.0, y + size - 2.0, x + size / 2.0, y + size - 2.0);
        g2d.fill(leaf);
        
        // Draw the vein
        g2d.setColor(new Color(255, 255, 255, 120)); // Fainter vein for older leaves
        g2d.setStroke(new BasicStroke(1.0f));
        g2d.drawLine((int)(x + size / 2.0), (int)(y + size - 4.0), (int)(x + size / 2.0), (int)(y + 4.0));

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
