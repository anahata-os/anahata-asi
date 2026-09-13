/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Path2D;

/**
 * A programmatically drawn pencil icon for edit actions.
 * <p>
 * Stylized with a Barça gold/yellow body, red eraser, silver ferrule,
 * and dark graphite tip angled at 45 degrees.
 * </p>
 *
 * @author anahata
 */
public class EditIcon extends AbstractAnahataIcon {

    /**
     * Constructs a new EditIcon with the specified size.
     * @param size The size in pixels.
     */
    public EditIcon(int size) {
        super(size);
    }

    /**
     * {@inheritDoc}
     * <p>Renders a 45-degree angled pencil with tip, body, ferrule, and eraser.</p>
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

        if (!c.isEnabled()) {
            g2d.setColor(Color.GRAY);
            g2d.rotate(Math.toRadians(-45), x + size / 2.0, y + size / 2.0);
            g2d.drawRect(x + size / 3, y + 2, size / 3, size - 4);
            g2d.dispose();
            return;
        }

        double s = size;
        g2d.translate(x, y);

        Color barcaYellow = new Color(255, 195, 0);
        Color barcaRed = new Color(165, 0, 68);
        Color wood = new Color(245, 215, 160);
        Color graphite = new Color(50, 50, 50);

        // Angle pencil at 45 degrees
        g2d.rotate(Math.toRadians(45), s / 2.0, s / 2.0);

        double w = s * 0.28;
        double cx = (s - w) / 2.0;

        // Eraser (top)
        g2d.setColor(barcaRed);
        g2d.fillRoundRect((int) cx, 1, (int) w, (int) (s * 0.20), 2, 2);

        // Ferrule / collar (metal band)
        g2d.setColor(new Color(180, 185, 195));
        g2d.fillRect((int) cx, (int) (s * 0.20), (int) w, (int) (s * 0.12));

        // Pencil shaft (body)
        g2d.setColor(barcaYellow);
        g2d.fillRect((int) cx, (int) (s * 0.32), (int) w, (int) (s * 0.38));

        // Subtle center seam line on pencil body
        g2d.setColor(new Color(230, 165, 0));
        g2d.drawLine((int) (s / 2.0), (int) (s * 0.32), (int) (s / 2.0), (int) (s * 0.70));

        // Sharpened wood cone
        Path2D.Double cone = new Path2D.Double();
        cone.moveTo(cx, s * 0.70);
        cone.lineTo(cx + w, s * 0.70);
        cone.lineTo(s / 2.0, s - 1);
        cone.closePath();
        g2d.setColor(wood);
        g2d.fill(cone);

        // Graphite tip
        Path2D.Double lead = new Path2D.Double();
        lead.moveTo(cx + w * 0.25, s * 0.83);
        lead.lineTo(cx + w * 0.75, s * 0.83);
        lead.lineTo(s / 2.0, s - 1);
        lead.closePath();
        g2d.setColor(graphite);
        g2d.fill(lead);

        // Subtle outline
        g2d.setColor(new Color(0, 0, 0, 50));
        g2d.setStroke(new BasicStroke(0.75f));
        g2d.drawRoundRect((int) cx, 1, (int) w, (int) (s * 0.20), 2, 2);
        g2d.drawRect((int) cx, (int) (s * 0.20), (int) w, (int) (s * 0.12));
        g2d.drawRect((int) cx, (int) (s * 0.32), (int) w, (int) (s * 0.38));
        g2d.draw(cone);

        g2d.dispose();
    }
}
