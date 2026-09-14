/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.net.URI;
import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * High-fidelity pure Swing image viewer implementing {@link MediaViewerComponent}.
 * <p>
 * Supports smooth interactive zoom (mouse wheel), mouse dragging to pan when zoomed,
 * double-click fit/1:1 toggle, integrated {@link MediaToolbar}, and right-click context actions.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class SwingImageViewer extends JPanel implements MediaViewerComponent {

    /** The action and metadata toolbar. */
    @Getter
    private final MediaToolbar toolbar = new MediaToolbar();

    /** The scroll pane enclosing the canvas. */
    private final JScrollPane scrollPane;

    /** The interactive image drawing canvas. */
    private final ImageCanvas canvas = new ImageCanvas();

    /** Decoded full-resolution image. */
    private BufferedImage image;

    /** Current zoom scale factor (1.0 = 100% 1:1). */
    private double scale = 1.0;

    /** Whether the image is currently fitted to the viewport dimensions. */
    private boolean fitToView = true;

    /**
     * Constructs a new SwingImageViewer.
     */
    public SwingImageViewer() {
        super(new BorderLayout());
        setOpaque(true);
        setBackground(new Color(24, 28, 36));

        scrollPane = new JScrollPane(canvas);
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        scrollPane.setOpaque(false);
        scrollPane.getViewport().setOpaque(false);
        scrollPane.getVerticalScrollBar().setUnitIncrement(16);
        scrollPane.getHorizontalScrollBar().setUnitIncrement(16);

        add(scrollPane, BorderLayout.CENTER);
        add(toolbar, BorderLayout.SOUTH);

        canvas.setToolTipText("Ctrl + Scroll to zoom in / out (Double-click to toggle 1:1)");

        // Bind toolbar image supplier for clipboard copy
        toolbar.setImageSupplier(() -> image);

        setupInteractions();
    }

    /**
     * Configures mouse wheel zoom, pan dragging, and context menu.
     */
    private void setupInteractions() {
        MouseAdapter adapter = new MouseAdapter() {
            private Point origin;

            @Override
            public void mousePressed(MouseEvent e) {
                if (SwingUtilities.isLeftMouseButton(e)) {
                    origin = new Point(e.getPoint());
                    canvas.setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
                } else if (e.isPopupTrigger()) {
                    showContextMenu(e);
                }
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                canvas.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                if (e.isPopupTrigger()) {
                    showContextMenu(e);
                }
            }

            @Override
            public void mouseDragged(MouseEvent e) {
                if (origin != null && scrollPane.getViewport() != null) {
                    int deltaX = origin.x - e.getX();
                    int deltaY = origin.y - e.getY();
                    Point viewPos = scrollPane.getViewport().getViewPosition();
                    viewPos.translate(deltaX, deltaY);
                    canvas.scrollRectToVisible(new java.awt.Rectangle(viewPos, scrollPane.getViewport().getSize()));
                }
            }

            @Override
            public void mouseClicked(MouseEvent e) {
                if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount() == 2) {
                    toggleFitOrActualSize();
                }
            }

            @Override
            public void mouseWheelMoved(MouseWheelEvent e) {
                if (e.isControlDown()) {
                    double zoomFactor = (e.getWheelRotation() < 0) ? 1.15 : 0.87;
                    zoom(zoomFactor, e.getPoint());
                    e.consume();
                } else {
                    if (getParent() != null) {
                        getParent().dispatchEvent(e);
                    }
                }
            }
        };

        canvas.addMouseListener(adapter);
        canvas.addMouseMotionListener(adapter);
        canvas.addMouseWheelListener(adapter);
    }

    /**
     * Toggles between fit-to-view and actual 1:1 pixel size.
     */
    public void toggleFitOrActualSize() {
        if (image == null) {
            return;
        }
        if (fitToView) {
            scale = 1.0;
            fitToView = false;
        } else {
            calculateFitScale();
            fitToView = true;
        }
        updateCanvasSize();
    }

    /**
     * Zooms the image by a multiplier centered on the given anchor point.
     *
     * @param factor Zoom factor.
     * @param anchor Mouse anchor point.
     */
    public void zoom(double factor, Point anchor) {
        if (image == null) {
            return;
        }
        fitToView = false;
        double newScale = Math.max(0.05, Math.min(20.0, scale * factor));
        if (Math.abs(newScale - scale) < 0.001) {
            return;
        }

        scale = newScale;
        updateCanvasSize();
    }

    /**
     * Computes the scale needed to fit the image inside the viewport.
     */
    private void calculateFitScale() {
        if (image == null || scrollPane.getViewport() == null) {
            return;
        }
        Dimension viewSize = scrollPane.getViewport().getSize();
        if (viewSize.width <= 0 || viewSize.height <= 0) {
            scale = 1.0;
            return;
        }
        double scaleX = (double) viewSize.width / image.getWidth();
        double scaleY = (double) viewSize.height / image.getHeight();
        scale = Math.min(scaleX, scaleY);
    }

    /**
     * Updates the canvas dimensions to match the scaled image size.
     */
    private void updateCanvasSize() {
        if (image == null) {
            return;
        }
        int w = (int) Math.round(image.getWidth() * scale);
        int h = (int) Math.round(image.getHeight() * scale);
        canvas.setPreferredSize(new Dimension(w, h));
        canvas.revalidate();
        canvas.repaint();
    }

    /**
     * Displays a popup context menu with common image actions.
     *
     * @param e Mouse event triggering the popup.
     */
    private void showContextMenu(MouseEvent e) {
        JPopupMenu menu = new JPopupMenu();

        JMenuItem copyItem = new JMenuItem("Copy Image");
        copyItem.addActionListener(ev -> toolbar.copyToClipboard());
        menu.add(copyItem);

        JMenuItem saveItem = new JMenuItem("Save As...");
        saveItem.addActionListener(ev -> toolbar.saveAs());
        menu.add(saveItem);

        menu.addSeparator();

        JMenuItem toggleFitItem = new JMenuItem(fitToView ? "Actual Size (1:1)" : "Fit to Window");
        toggleFitItem.addActionListener(ev -> toggleFitOrActualSize());
        menu.add(toggleFitItem);

        if (toolbar.getSourceUri() != null) {
            JMenuItem openExtItem = new JMenuItem("Open in System Viewer");
            openExtItem.addActionListener(ev -> toolbar.openExternal());
            menu.add(openExtItem);
        }

        menu.show(canvas, e.getX(), e.getY());
    }

    /**
     * {@inheritDoc}
     * <p>Returns this JPanel as the primary visual component.</p>
     */
    @Override
    public JComponent getComponent() {
        return this;
    }

    /**
     * {@inheritDoc}
     * <p>Decodes image bytes and updates metadata and toolbar.</p>
     */
    @Override
    public void load(byte[] data, String mimeType, String displayName, URI sourceUri) {
        toolbar.setData(data);
        toolbar.setMimeType(mimeType);
        toolbar.setDisplayName(displayName);
        toolbar.setSourceUri(sourceUri);

        if (data != null && data.length > 0) {
            try {
                this.image = ImageIO.read(new ByteArrayInputStream(data));
                if (image != null) {
                    String dim = image.getWidth() + " × " + image.getHeight();
                    toolbar.updateMetadata(dim);
                    calculateFitScale();
                    updateCanvasSize();
                } else {
                    toolbar.updateMetadata("Decode Error");
                }
            } catch (Exception ex) {
                log.error("Failed to decode image data", ex);
                toolbar.updateMetadata("Error");
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>No active audio or video playback in image viewer.</p>
     */
    @Override
    public void stop() {
    }

    /**
     * {@inheritDoc}
     * <p>Releases image buffer.</p>
     */
    @Override
    public void dispose() {
        this.image = null;
    }

    /**
     * Interactive canvas rendering the scaled image centered in the viewport.
     */
    private class ImageCanvas extends JPanel {
        public ImageCanvas() {
            setOpaque(false);
        }

        @Override
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            if (image == null) {
                return;
            }

            Graphics2D g2 = (Graphics2D) g.create();
            g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
            g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            int drawW = (int) Math.round(image.getWidth() * scale);
            int drawH = (int) Math.round(image.getHeight() * scale);

            int x = Math.max(0, (getWidth() - drawW) / 2);
            int y = Math.max(0, (getHeight() - drawH) / 2);

            g2.drawImage(image, x, y, drawW, drawH, null);
            g2.dispose();
        }
    }
}
