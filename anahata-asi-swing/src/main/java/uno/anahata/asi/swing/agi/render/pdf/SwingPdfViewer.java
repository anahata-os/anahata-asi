/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.render.pdf;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Insets;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.net.URI;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.DefaultListModel;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSpinner;
import javax.swing.JToggleButton;
import javax.swing.ListSelectionModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.apache.pdfbox.rendering.PDFRenderer;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.render.MediaToolbar;
import uno.anahata.asi.swing.agi.render.MediaViewerComponent;
import uno.anahata.asi.swing.icons.ActualSizeIcon;
import uno.anahata.asi.swing.icons.FitWidthIcon;
import uno.anahata.asi.swing.icons.SidebarSplitIcon;
import uno.anahata.asi.swing.icons.ZoomMinusIcon;
import uno.anahata.asi.swing.icons.ZoomPlusIcon;

/**
 * Universal, high-performance pure-Java PDF document viewer component.
 * <p>
 * Implements {@link MediaViewerComponent} backed by Apache PDFBox. Provides
 * asynchronous, proportional thumbnail navigation, interactive zooming, dynamic
 * fit-to-width tracking, and integration with the standard {@link MediaToolbar}.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class SwingPdfViewer extends JPanel implements MediaViewerComponent {

    /**
     * Parent AgiPanel providing session and theming configuration.
     */
    private final AgiPanel agiPanel;

    /**
     * Standard action and metadata toolbar docked at the bottom.
     */
    @Getter
    private final MediaToolbar toolbar;

    /**
     * Loaded raw PDF document bytes.
     */
    private byte[] pdfData;

    /**
     * Display name or file name of the active document.
     */
    private String documentName;

    /**
     * Source URI of the document on disk or network.
     */
    private URI documentUri;

    /**
     * Active Apache PDFBox document instance.
     */
    private PDDocument document;

    /**
     * PDFBox raster rendering engine.
     */
    private PDFRenderer renderer;

    /**
     * Total number of pages in the loaded document.
     */
    private int totalPages = 0;

    /**
     * Currently active 0-based page index.
     */
    private int currentPage = 0;

    /**
     * Active zoom multiplier (1.0 = 100% 150 DPI baseline).
     */
    private double zoomFactor = 1.0;

    /**
     * Last observed main viewport width used to detect resize events.
     */
    private int lastViewportWidth = -1;

    /**
     * In-memory cache for rendered full-page rasters keyed by page index.
     */
    private final Map<Integer, BufferedImage> pageCache = new ConcurrentHashMap<>();

    /**
     * In-memory cache for generated thumbnail previews keyed by page index.
     */
    private final Map<Integer, BufferedImage> thumbnailCache = new ConcurrentHashMap<>();

    /**
     * Background worker thread responsible for progressive thumbnail generation.
     */
    private Thread thumbnailLoaderThread;

    /**
     * Toggle button to collapse or expand the page thumbnail navigator.
     */
    private final JToggleButton sidebarToggle = createSquareToggle(new SidebarSplitIcon(16), "Toggle Page Thumbnails Sidebar");

    /**
     * Jump button to navigate directly to the first page.
     */
    private final JButton firstBtn = createSquareBtn("⏮", "First Page");

    /**
     * Button to navigate to the previous page.
     */
    private final JButton prevBtn = createSquareBtn("◀", "Previous Page");

    /**
     * Spinner control for entering or stepping through page numbers.
     */
    private final JSpinner pageSpinner = new JSpinner(new SpinnerNumberModel(1, 1, 1, 1));

    /**
     * Label showing total pages count.
     */
    private final JLabel pageInfoLabel = new JLabel("/ 1");

    /**
     * Button to navigate to the next page.
     */
    private final JButton nextBtn = createSquareBtn("▶", "Next Page");

    /**
     * Jump button to navigate directly to the last page.
     */
    private final JButton lastBtn = createSquareBtn("⏭", "Last Page");

    /**
     * Toggle button to lock zoom to the viewport width.
     */
    private final JToggleButton fitWidthToggle = createSquareToggle(new FitWidthIcon(16), "Fit Page to Window Width");

    /**
     * Button to reset zoom to 100% actual size.
     */
    private final JButton actualSizeBtn = createSquareBtn(new ActualSizeIcon(14), "Actual Size (100% 1:1)");

    /**
     * Button to zoom out.
     */
    private final JButton zoomOutBtn = createSquareBtn(new ZoomMinusIcon(14), "Zoom Out (-)");

    /**
     * Button to zoom in.
     */
    private final JButton zoomInBtn = createSquareBtn(new ZoomPlusIcon(14), "Zoom In (+)");

    /**
     * Label displaying the current zoom percentage.
     */
    private final JLabel zoomLabel = new JLabel("100%");

    /**
     * Collapsible sidebar panel holding the page thumbnail list.
     */
    private final JPanel sidebarPanel = new JPanel(new BorderLayout());

    /**
     * Data model for the page thumbnails JList.
     */
    private final DefaultListModel<Integer> thumbnailListModel = new DefaultListModel<>();

    /**
     * JList component presenting the page thumbnails.
     */
    private final JList<Integer> thumbnailList = new JList<>(thumbnailListModel);

    /**
     * Scroll pane enclosing the thumbnail navigator.
     */
    private final JScrollPane thumbnailScrollPane;

    /**
     * Scroll pane enclosing the active page canvas.
     */
    private final JScrollPane mainScrollPane;

    /**
     * Drawing canvas rendering the active page.
     */
    private final PdfPageCanvas canvas = new PdfPageCanvas();

    /**
     * Constructs a new SwingPdfViewer bound to an AgiPanel.
     *
     * @param agiPanel The parent AgiPanel providing configuration and session context.
     */
    public SwingPdfViewer(@NonNull AgiPanel agiPanel) {
        super(new BorderLayout());
        this.agiPanel = agiPanel;
        this.toolbar = new MediaToolbar(agiPanel);

        setOpaque(true);
        setBackground(new Color(24, 28, 36));

        pageSpinner.setPreferredSize(new Dimension(60, 26));

        // Setup top control toolbar
        add(createTopToolbar(), BorderLayout.NORTH);

        // Center split area: Sidebar (Left) + Page Viewport (Center)
        JPanel centerContainer = new JPanel(new BorderLayout());
        centerContainer.setOpaque(false);

        // Sidebar setup
        setupThumbnailList();
        thumbnailScrollPane = new JScrollPane(thumbnailList);
        thumbnailScrollPane.setBorder(BorderFactory.createMatteBorder(0, 0, 0, 1, new Color(45, 52, 65)));
        thumbnailScrollPane.setPreferredSize(new Dimension(150, 450));
        thumbnailScrollPane.getVerticalScrollBar().setUnitIncrement(24);
        sidebarPanel.add(thumbnailScrollPane, BorderLayout.CENTER);
        centerContainer.add(sidebarPanel, BorderLayout.WEST);

        // Main canvas scroll pane setup
        mainScrollPane = new JScrollPane(canvas);
        mainScrollPane.setBorder(BorderFactory.createEmptyBorder());
        mainScrollPane.setOpaque(false);
        mainScrollPane.getViewport().setOpaque(false);
        mainScrollPane.setPreferredSize(new Dimension(650, 450));
        mainScrollPane.getVerticalScrollBar().setUnitIncrement(24);
        mainScrollPane.getHorizontalScrollBar().setUnitIncrement(24);
        centerContainer.add(mainScrollPane, BorderLayout.CENTER);

        add(centerContainer, BorderLayout.CENTER);
        add(toolbar, BorderLayout.SOUTH);

        // Bind image supplier to toolbar for clipboard copy
        toolbar.setImageSupplier(canvas::getImage);

        setupListeners();
        updateControlsState();
    }

    /**
     * Creates and styles the top navigation and zoom control bar.
     *
     * @return The configured top toolbar JPanel.
     */
    private JPanel createTopToolbar() {
        JPanel bar = new JPanel(new BorderLayout(4, 0));
        bar.setBackground(new Color(30, 34, 44));
        bar.setBorder(new EmptyBorder(4, 8, 4, 8));

        JPanel leftNav = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 0));
        leftNav.setOpaque(false);
        sidebarToggle.setSelected(true);
        leftNav.add(sidebarToggle);

        JSeparator sep = new JSeparator(SwingConstants.VERTICAL);
        sep.setPreferredSize(new Dimension(1, 18));
        sep.setForeground(new Color(55, 65, 82));
        leftNav.add(sep);

        leftNav.add(firstBtn);
        leftNav.add(prevBtn);

        pageSpinner.setPreferredSize(new Dimension(45, 24));
        leftNav.add(pageSpinner);

        pageInfoLabel.setFont(new Font("SansSerif", Font.PLAIN, 11));
        pageInfoLabel.setForeground(new Color(180, 190, 210));
        leftNav.add(pageInfoLabel);

        leftNav.add(nextBtn);
        leftNav.add(lastBtn);

        JPanel rightZoom = new JPanel(new FlowLayout(FlowLayout.RIGHT, 3, 0));
        rightZoom.setOpaque(false);

        fitWidthToggle.setSelected(true);
        rightZoom.add(fitWidthToggle);
        rightZoom.add(actualSizeBtn);
        rightZoom.add(zoomOutBtn);
        rightZoom.add(zoomInBtn);

        zoomLabel.setFont(new Font("SansSerif", Font.BOLD, 10));
        zoomLabel.setForeground(new Color(160, 175, 200));
        zoomLabel.setPreferredSize(new Dimension(36, 24));
        zoomLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        rightZoom.add(zoomLabel);

        bar.add(leftNav, BorderLayout.WEST);
        bar.add(rightZoom, BorderLayout.EAST);
        return bar;
    }

    /**
     * Helper to create a compact 26x26 square button with tooltip.
     */
    private static JButton createSquareBtn(Object iconOrText, String tooltip) {
        JButton btn = new JButton();
        btn.setPreferredSize(new Dimension(26, 26));
        btn.setMargin(new Insets(1, 1, 1, 1));
        btn.setFocusable(false);
        btn.setToolTipText(tooltip);
        if (iconOrText instanceof Icon ic) {
            btn.setIcon(ic);
        } else {
            btn.setText(iconOrText.toString());
            btn.setFont(new Font("SansSerif", Font.PLAIN, 10));
        }
        return btn;
    }

    /**
     * Helper to create a compact 26x26 square toggle button with tooltip.
     */
    private static JToggleButton createSquareToggle(Icon icon, String tooltip) {
        JToggleButton btn = new JToggleButton(icon);
        btn.setPreferredSize(new Dimension(26, 26));
        btn.setMargin(new Insets(1, 1, 1, 1));
        btn.setFocusable(false);
        btn.setToolTipText(tooltip);
        return btn;
    }

    /**
     * Initializes thumbnail list models, renderers, and selection listeners.
     */
    private void setupThumbnailList() {
        thumbnailList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        thumbnailList.setBackground(new Color(22, 26, 34));
        thumbnailList.setFixedCellWidth(135);
        thumbnailList.setFixedCellHeight(165);
        thumbnailList.setCellRenderer(new PdfThumbnailCellRenderer(thumbnailCache));

        thumbnailList.addListSelectionListener(e -> {
            if (!e.getValueIsAdjusting()) {
                int selected = thumbnailList.getSelectedIndex();
                if (selected >= 0 && selected != currentPage) {
                    goToPage(selected);
                }
            }
        });

        thumbnailList.addMouseWheelListener(e -> {
            JScrollBar vBar = thumbnailScrollPane.getVerticalScrollBar();
            if (vBar != null && vBar.isVisible()) {
                int units = e.getUnitsToScroll();
                int amount = units * vBar.getUnitIncrement();
                vBar.setValue(vBar.getValue() + amount);
                e.consume();
            }
        });
    }

    /**
     * Attaches user action and viewport resize listeners.
     */
    private void setupListeners() {
        sidebarToggle.addActionListener(e -> {
            sidebarPanel.setVisible(sidebarToggle.isSelected());
            revalidate();
            repaint();
            if (fitWidthToggle.isSelected()) {
                SwingUtilities.invokeLater(this::checkResizeAndRefit);
            }
        });

        firstBtn.addActionListener(e -> goToPage(0));
        prevBtn.addActionListener(e -> goToPage(currentPage - 1));
        nextBtn.addActionListener(e -> goToPage(currentPage + 1));
        lastBtn.addActionListener(e -> goToPage(totalPages - 1));

        pageSpinner.addChangeListener(e -> {
            int target = ((Number) pageSpinner.getValue()).intValue() - 1;
            if (target != currentPage) {
                goToPage(target);
            }
        });

        fitWidthToggle.addActionListener(e -> setFitWidthMode(fitWidthToggle.isSelected()));

        actualSizeBtn.addActionListener(e -> {
            setFitWidthMode(false);
            zoomFactor = 1.0;
            pageCache.clear();
            renderCurrentPage();
        });

        zoomInBtn.addActionListener(e -> {
            setFitWidthMode(false);
            zoomFactor = Math.min(4.0, zoomFactor * 1.25);
            pageCache.clear();
            renderCurrentPage();
        });

        zoomOutBtn.addActionListener(e -> {
            setFitWidthMode(false);
            zoomFactor = Math.max(0.25, zoomFactor / 1.25);
            pageCache.clear();
            renderCurrentPage();
        });

        // Viewport resize tracking for dynamic fit-width adjustment
        mainScrollPane.getViewport().addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent e) {
                if (fitWidthToggle.isSelected()) {
                    checkResizeAndRefit();
                }
            }
        });

        // Mouse Wheel handling on canvas: Ctrl+Wheel to Zoom, regular Wheel to Scroll or Page Turn
        canvas.addMouseWheelListener(e -> {
            if (e.isControlDown()) {
                setFitWidthMode(false);
                if (e.getWheelRotation() < 0) {
                    zoomFactor = Math.min(4.0, zoomFactor * 1.15);
                } else {
                    zoomFactor = Math.max(0.25, zoomFactor / 1.15);
                }
                pageCache.clear();
                renderCurrentPage();
                e.consume();
            } else {
                JScrollBar vBar = mainScrollPane.getVerticalScrollBar();
                if (vBar != null && vBar.isVisible()) {
                    int units = e.getUnitsToScroll();
                    int amount = units * vBar.getUnitIncrement();
                    int oldVal = vBar.getValue();
                    vBar.setValue(oldVal + amount);
                    if (vBar.getValue() != oldVal) {
                        e.consume();
                        return;
                    }
                }
                // When at the top or bottom boundary (or page fits viewport), wheel flips to previous/next page
                if (e.getWheelRotation() > 0 && currentPage < totalPages - 1) {
                    goToPage(currentPage + 1);
                    e.consume();
                } else if (e.getWheelRotation() < 0 && currentPage > 0) {
                    goToPage(currentPage - 1);
                    e.consume();
                }
            }
        });
    }

    /**
     * Toggles fit-width mode and updates control button states accordingly.
     *
     * @param fit True to lock zoom to viewport width, false for manual zoom.
     */
    private void setFitWidthMode(boolean fit) {
        fitWidthToggle.setSelected(fit);
        actualSizeBtn.setEnabled(!fit);
        zoomInBtn.setEnabled(!fit);
        zoomOutBtn.setEnabled(!fit);

        if (fit) {
            pageCache.clear();
            renderCurrentPage();
        }
    }

    /**
     * Checks if the viewport width has changed and refits the page if necessary.
     */
    private void checkResizeAndRefit() {
        int currentW = mainScrollPane.getViewport().getWidth();
        if (currentW > 50 && Math.abs(currentW - lastViewportWidth) > 5) {
            lastViewportWidth = currentW;
            pageCache.clear();
            renderCurrentPage();
        }
    }

    /**
     * Navigates to a specific 0-based page index.
     *
     * @param page Target 0-based page index.
     */
    public void goToPage(int page) {
        if (page < 0 || page >= totalPages || page == currentPage) {
            return;
        }
        this.currentPage = page;
        this.pageSpinner.setValue(page + 1);
        thumbnailList.setSelectedIndex(page);
        thumbnailList.ensureIndexIsVisible(page);

        updateControlsState();
        renderCurrentPage();
    }

    /**
     * Refreshes the enabled status of navigation buttons and page counter text.
     */
    private void updateControlsState() {
        firstBtn.setEnabled(currentPage > 0);
        prevBtn.setEnabled(currentPage > 0);
        nextBtn.setEnabled(currentPage < totalPages - 1);
        lastBtn.setEnabled(currentPage < totalPages - 1);
        pageInfoLabel.setText("/ " + Math.max(1, totalPages));
        zoomLabel.setText((int) Math.round(zoomFactor * 100) + "%");
    }

    /**
     * Renders the current page to the canvas using cached or newly generated raster.
     */
    public void renderCurrentPage() {
        if (document == null || totalPages == 0) {
            return;
        }

        try {
            float dpi;
            if (fitWidthToggle.isSelected()) {
                int viewW = mainScrollPane.getViewport().getWidth();
                if (viewW <= 0) {
                    viewW = 800;
                }
                int targetW = Math.max(250, viewW - 48);

                PDRectangle crop = document.getPage(currentPage).getCropBox();
                float scale = (float) targetW / crop.getWidth();
                dpi = Math.max(50f, Math.min(300f, 72f * scale));
                zoomFactor = scale;
                lastViewportWidth = viewW;
            } else {
                dpi = (float) (150f * zoomFactor);
            }

            zoomLabel.setText((int) Math.round(zoomFactor * 100) + "%");

            BufferedImage img = pageCache.get(currentPage);
            if (img == null) {
                img = renderer.renderImageWithDPI(currentPage, dpi);
                pageCache.put(currentPage, img);
            }

            canvas.setImage(img);
        } catch (Exception ex) {
            log.error("Failed to render PDF page {}: {}", currentPage, ex.getMessage(), ex);
        }
    }

    /**
     * Starts an asynchronous background daemon thread to render thumbnails progressively.
     */
    private void startBackgroundThumbnailLoading() {
        stopThumbnailLoader();

        thumbnailLoaderThread = new Thread(() -> {
            for (int i = 0; i < totalPages; i++) {
                if (Thread.currentThread().isInterrupted() || document == null) {
                    break;
                }
                try {
                    PDRectangle cropBox = document.getPage(i).getCropBox();
                    float pageWidthPt = cropBox.getWidth();
                    float pageHeightPt = cropBox.getHeight();

                    // Proportional scale to fit within 100x135 box
                    float scaleW = 100f / pageWidthPt;
                    float scaleH = 135f / pageHeightPt;
                    float scale = Math.min(scaleW, scaleH);
                    float thumbDpi = scale * 72f;

                    BufferedImage thumbImg = renderer.renderImageWithDPI(i, thumbDpi);
                    thumbnailCache.put(i, thumbImg);

                    final int pageIdx = i;
                    SwingUtilities.invokeLater(() -> {
                        if (pageIdx < thumbnailListModel.size()) {
                            thumbnailList.repaint(thumbnailList.getCellBounds(pageIdx, pageIdx));
                        }
                    });
                } catch (Exception e) {
                    log.debug("Background thumbnail generation skipped for page {}: {}", i, e.getMessage());
                }
            }
        }, "PdfThumbnailLoader");

        thumbnailLoaderThread.setDaemon(true);
        thumbnailLoaderThread.start();
    }

    /**
     * Safely interrupts and stops the background thumbnail loader thread.
     */
    private void stopThumbnailLoader() {
        if (thumbnailLoaderThread != null && thumbnailLoaderThread.isAlive()) {
            thumbnailLoaderThread.interrupt();
            thumbnailLoaderThread = null;
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns a bounded preferred size (maximum 750px height) so that the viewer
     * remains comfortably docked inside tab panes and scrollable containers without
     * inflating to the unclipped height of the full PDF document page.
     * </p>
     */
    @Override
    public Dimension getPreferredSize() {
        Dimension d = super.getPreferredSize();
        int h = Math.max(450, Math.min(720, d.height));
        return new Dimension(d.width, h);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns this panel as the primary JComponent for embedding.
     * </p>
     */
    @Override
    public JComponent getComponent() {
        return this;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Decodes the PDF byte stream using Apache PDFBox, populates navigation models,
     * updates toolbar metadata, and kicks off page rendering.
     * </p>
     */
    @Override
    public void load(byte[] data, String mimeType, String displayName, URI sourceUri) {
        disposeDocument();

        this.pdfData = data;
        this.documentName = displayName;
        this.documentUri = sourceUri;

        toolbar.setData(data);
        toolbar.setMimeType(mimeType != null ? mimeType : "application/pdf");
        toolbar.setDisplayName(displayName != null ? displayName : "Document.pdf");
        toolbar.setSourceUri(sourceUri);

        if (data == null || data.length == 0) {
            toolbar.updateMetadata("Empty PDF");
            return;
        }

        try {
            this.document = PDDocument.load(new ByteArrayInputStream(data));
            this.renderer = new PDFRenderer(document);
            this.totalPages = document.getNumberOfPages();
            this.currentPage = 0;

            pageSpinner.setModel(new SpinnerNumberModel(1, 1, Math.max(1, totalPages), 1));

            thumbnailListModel.clear();
            for (int i = 0; i < totalPages; i++) {
                thumbnailListModel.addElement(i);
            }
            if (totalPages > 0) {
                thumbnailList.setSelectedIndex(0);
            }

            toolbar.updateMetadata(totalPages + " pages");
            updateControlsState();

            pageCache.clear();
            thumbnailCache.clear();

            renderCurrentPage();
            startBackgroundThumbnailLoading();
        } catch (Exception ex) {
            log.error("Failed to load PDF document '{}': {}", displayName, ex.getMessage(), ex);
            toolbar.updateMetadata("PDF Load Error");
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * PDF documents have no active media playback streams to stop.
     * </p>
     */
    @Override
    public void stop() {
        // No audio/video playback streams to stop
    }

    /**
     * Closes the active PDDocument and releases caches.
     */
    private void disposeDocument() {
        stopThumbnailLoader();
        pageCache.clear();
        thumbnailCache.clear();

        if (document != null) {
            try {
                document.close();
            } catch (Exception e) {
                log.debug("Error closing PDDocument: {}", e.getMessage());
            }
            document = null;
        }
        renderer = null;
        totalPages = 0;
        currentPage = 0;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Cleans up background threads, cached page rasters, and closes the PDDocument.
     * </p>
     */
    @Override
    public void dispose() {
        disposeDocument();
        canvas.setImage(null);
    }
}
