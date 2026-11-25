/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.render.pdf;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.util.Map;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.ListCellRenderer;

/**
 * High-fidelity, proportional thumbnail cell renderer for the PDF page navigator.
 * <p>
 * Renders each page preview centered within its cell while strictly preserving the
 * true aspect ratio of the underlying PDF page. Applies drop shadows, white paper
 * background, crisp page outlines, dynamic selection borders, and a clean page number badge.
 * </p>
 *
 * @author anahata
 */
public class PdfThumbnailCellRenderer extends JPanel implements ListCellRenderer<Integer> {

    /**
     * Cache supplying rendered page thumbnails keyed by 0-based page index.
     */
    private final Map<Integer, BufferedImage> thumbnailCache;

    /**
     * Currently rendered 0-based page index.
     */
    private int pageIndex = 0;

    /**
     * Whether the cell is currently selected in the JList.
     */
    private boolean isSelected = false;

    /**
     * Standard background color for unselected thumbnail cells.
     */
    private final Color unselectedBg = new Color(22, 26, 34);

    /**
     * Standard background color for selected thumbnail cells.
     */
    private final Color selectedBg = new Color(36, 44, 58);

    /**
     * Accent border color for selected page cells.
     */
    private final Color selectionBorderColor = new Color(59, 130, 246);

    /**
     * Border color for unselected page cells.
     */
    private final Color defaultBorderColor = new Color(80, 88, 102);

    /**
     * Constructs a new PdfThumbnailCellRenderer bound to the thumbnail cache.
     *
     * @param thumbnailCache The map holding generated thumbnail images.
     */
    public PdfThumbnailCellRenderer(Map<Integer, BufferedImage> thumbnailCache) {
        this.thumbnailCache = thumbnailCache;
        setOpaque(true);
        setPreferredSize(new Dimension(135, 165));
    }

    /**
     * {@inheritDoc}
     * <p>
     * Updates internal cell state for the given page index and selection status.
     * </p>
     */
    @Override
    public Component getListCellRendererComponent(JList<? extends Integer> list, Integer value, int index, boolean isSelected, boolean cellHasFocus) {
        this.pageIndex = (value != null) ? value : index;
        this.isSelected = isSelected;
        setBackground(isSelected ? selectedBg : unselectedBg);
        return this;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Paints the page thumbnail centered, maintaining true aspect ratio with paper
     * drop shadow, crisp border, and the page number label below.
     * </p>
     */
    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);

        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);

        int panelW = getWidth();
        int panelH = getHeight();

        BufferedImage thumb = (thumbnailCache != null) ? thumbnailCache.get(pageIndex) : null;

        if (thumb != null) {
            int imgW = thumb.getWidth();
            int imgH = thumb.getHeight();

            // Center thumbnail horizontally and vertically within the upper 135px preview area
            int x = (panelW - imgW) / 2;
            int y = 8 + (130 - imgH) / 2;

            // Soft drop shadow
            g2.setColor(new Color(0, 0, 0, 90));
            g2.fillRoundRect(x - 2, y - 1, imgW + 4, imgH + 4, 4, 4);

            // White paper sheet background
            g2.setColor(Color.WHITE);
            g2.fillRect(x, y, imgW, imgH);

            // Render scaled thumbnail
            g2.drawImage(thumb, x, y, null);

            // Page border outline
            if (isSelected) {
                g2.setColor(selectionBorderColor);
                g2.setStroke(new BasicStroke(2f));
                g2.drawRect(x - 1, y - 1, imgW + 1, imgH + 1);
            } else {
                g2.setColor(defaultBorderColor);
                g2.setStroke(new BasicStroke(1f));
                g2.drawRect(x, y, imgW, imgH);
            }
        } else {
            // Placeholder box while rendering
            int w = 85;
            int h = 120;
            int x = (panelW - w) / 2;
            int y = 14;
            g2.setColor(new Color(30, 36, 46));
            g2.fillRect(x, y, w, h);
            g2.setColor(new Color(100, 110, 125));
            g2.drawString("...", x + 35, y + 65);
        }

        // Bottom Page Number Label
        String label = "Page " + (pageIndex + 1);
        g2.setFont(new Font("SansSerif", isSelected ? Font.BOLD : Font.PLAIN, 11));
        FontMetrics fm = g2.getFontMetrics();
        int textW = fm.stringWidth(label);
        int textX = (panelW - textW) / 2;
        int textY = panelH - 6;

        g2.setColor(isSelected ? Color.WHITE : new Color(160, 172, 190));
        g2.drawString(label, textX, textY);

        g2.dispose();
    }
}
