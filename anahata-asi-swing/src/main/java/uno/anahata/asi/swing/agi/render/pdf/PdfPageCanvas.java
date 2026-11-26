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
 * crisp rendering at any scale. Supports high-performance Java2D scaling of a
 * master raster during live viewport resizes to guarantee smooth 60fps tracking.
 * </p>
 *
 * @author anahata
 */
public class PdfPageCanvas extends JPanel {

    /**
     * Currently rendered master page raster image.
     */
    private BufferedImage pageImage;

    /**
     * Target display width in pixels.
     */
    private int targetWidth = 0;

    /**
     * Target display height in pixels.
     */
    private int targetHeight = 0;

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
     * Updates the active master page image and display dimensions.
     *
     * @param image The newly rendered page master image, or null to clear.
     * @param displayWidth Target rendered width on screen.
     * @param displayHeight Target rendered height on screen.
     */
    public void setMasterImage(BufferedImage image, int displayWidth, int displayHeight) {
        this.pageImage = image;
        setDisplaySize(displayWidth, displayHeight);
    }

    /**
     * Updates only the display dimensions for instantaneous Java2D scaling of the master image.
     *
     * @param displayWidth Target rendered width on screen.
     * @param displayHeight Target rendered height on screen.
     */
    public void setDisplaySize(int displayWidth, int displayHeight) {
        this.targetWidth = displayWidth;
        this.targetHeight = displayHeight;
        if (targetWidth > 0 && targetHeight > 0) {
            setPreferredSize(new Dimension(targetWidth + (margin * 2), targetHeight + (margin * 2)));
        } else if (pageImage != null) {
            this.targetWidth = pageImage.getWidth();
            this.targetHeight = pageImage.getHeight();
            setPreferredSize(new Dimension(targetWidth + (margin * 2), targetHeight + (margin * 2)));
        } else {
            setPreferredSize(new Dimension(200, 200));
        }
        revalidate();
        repaint();
    }

    /**
     * Legacy compatibility setter for setting image with native dimensions.
     *
     * @param image The newly rendered page image, or null to clear.
     */
    public void setImage(BufferedImage image) {
        if (image != null) {
            setMasterImage(image, image.getWidth(), image.getHeight());
        } else {
            setMasterImage(null, 0, 0);
        }
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
     * crisp paper outline, and the scaled page raster using high-quality bilinear interpolation.
     * </p>
     */
    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);

        if (pageImage == null || targetWidth <= 0 || targetHeight <= 0) {
            return;
        }

        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        // Center horizontally in viewport
        int x = Math.max(margin, (getWidth() - targetWidth) / 2);
        int y = margin;

        // Draw soft drop shadow
        g2.setColor(shadowColor);
        g2.fillRoundRect(x - 4, y - 2, targetWidth + 8, targetHeight + 8, 8, 8);

        // Draw white paper base
        g2.setColor(Color.WHITE);
        g2.fillRect(x, y, targetWidth, targetHeight);

        // Draw scaled page graphics
        g2.drawImage(pageImage, x, y, targetWidth, targetHeight, null);

        // Draw fine paper boundary outline
        g2.setColor(paperBorderColor);
        g2.drawRect(x, y, targetWidth, targetHeight);

        g2.dispose();
    }
}
