/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.render.pdf;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import javax.swing.JPanel;

/**
 * Interactive Swing canvas for drawing rendered high-resolution PDF pages.
 * <p>
 * Paints the current page centered horizontally within the viewport, applying
 * a subtle drop shadow, realistic paper outline, and bilinear interpolation for
 * crisp rendering at any scale.
 * </p>
 *
 * @author anahata
 */
public class PdfPageCanvas extends JPanel {

    /**
     * Currently rendered page raster image.
     */
    private BufferedImage pageImage;

    /**
     * Outer padding border surrounding the rendered page paper.
     */
    private final int margin = 20;

    /**
     * Drop shadow translucent color.
     */
    private final Color shadowColor = new Color(0, 0, 0, 95);

    /**
     * Subtle paper border outline color.
     */
    private final Color paperBorderColor = new Color(175, 182, 195);

    /**
     * Constructs a new transparent PdfPageCanvas.
     */
    public PdfPageCanvas() {
        setOpaque(false);
    }

    /**
     * Updates the active page image and recalculates the canvas preferred size.
     *
     * @param image The newly rendered page image, or null to clear.
     */
    public void setImage(BufferedImage image) {
        this.pageImage = image;
        if (image != null) {
            setPreferredSize(new Dimension(image.getWidth() + (margin * 2), image.getHeight() + (margin * 2)));
        } else {
            setPreferredSize(new Dimension(200, 200));
        }
        revalidate();
        repaint();
    }

    /**
     * Gets the currently displayed page image.
     *
     * @return The active page image, or null if none loaded.
     */
    public BufferedImage getImage() {
        return pageImage;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Paints the page paper sheet centered horizontally with a drop shadow,
     * crisp paper outline, and the rendered page raster.
     * </p>
     */
    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);

        if (pageImage == null) {
            return;
        }

        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        int imgW = pageImage.getWidth();
        int imgH = pageImage.getHeight();

        // Center horizontally in viewport
        int x = Math.max(margin, (getWidth() - imgW) / 2);
        int y = margin;

        // Draw soft drop shadow
        g2.setColor(shadowColor);
        g2.fillRoundRect(x - 4, y - 2, imgW + 8, imgH + 8, 8, 8);

        // Draw white paper base
        g2.setColor(Color.WHITE);
        g2.fillRect(x, y, imgW, imgH);

        // Draw rendered page graphics
        g2.drawImage(pageImage, x, y, null);

        // Draw fine paper boundary outline
        g2.setColor(paperBorderColor);
        g2.drawRect(x, y, imgW, imgH);

        g2.dispose();
    }
}
