/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import javax.swing.UIManager;

/**
 * A programmatically drawn Sidebar Split-Panel icon.
 * <p>
 * Displays a window frame rectangle with a highlighted left sidebar panel,
 * symbolizing toggling document navigation panels or thumbnail sidebars.
 * </p>
 *
 * @author anahata
 */
public class SidebarSplitIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new SidebarSplitIcon of the specified square size.
     *
     * @param size The size of the icon in pixels.
     */
    public SidebarSplitIcon(int size) {
        super(size);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Paints an outer frame with a filled, accented left sidebar column.
     * </p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            int w = size - 3;
            int h = size - 3;
            int ox = x + 1;
            int oy = y + 1;

            Color frameColor = UIManager.getColor("Button.foreground");
            if (frameColor == null) {
                frameColor = (c != null) ? c.getForeground() : Color.LIGHT_GRAY;
            }

            // Outer frame
            g2.setColor(c != null && !c.isEnabled() ? Color.GRAY : frameColor);
            g2.setStroke(new BasicStroke(1.2f));
            g2.drawRoundRect(ox, oy, w, h, 2, 2);

            // Left sidebar pane (filled with accent blue)
            int sideW = (int) Math.round(w * 0.38);
            Color accentBlue = getBlueColor(c);
            g2.setColor(c != null && !c.isEnabled() ? Color.DARK_GRAY : accentBlue);
            g2.fillRect(ox + 1, oy + 1, sideW, h - 1);
        } finally {
            g2.dispose();
        }
    }
}
