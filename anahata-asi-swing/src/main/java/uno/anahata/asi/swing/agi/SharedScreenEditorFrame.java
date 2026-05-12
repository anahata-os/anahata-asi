/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.RegionSelectionIcon;
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.swing.internal.UICapture;
import uno.anahata.asi.swing.toolkit.Screens;

/**
 * A control panel for managing live screen sharing and region selection.
 * <p>
 * This frame provides a visual overview of all available graphics devices and 
 * active shared regions, allowing the user to toggle multimodal context in real-time.
 * It uses asynchronous background tasks to capture thumbnails, ensuring the UI 
 * remains responsive even under display server constraints (Wayland/Pipewire).
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class SharedScreenEditorFrame extends JFrame {

    /** The active AGI session context. */
    private final AgiPanel agiPanel;
    private final Agi agi;
    /** The specialized screens toolkit providing hardware access. */
    private final Screens screensToolkit;
    
    /** Container for the monitor preview components. */
    private JPanel monitorsContainer;
    /** Panel listing all currently active shared regions. */
    private JPanel regionsListPanel;
    /** Cache for monitor thumbnails to improve rendering performance. */
    private final Map<Integer, Image> monitorThumbnails = new HashMap<>();
    /** The standardized height for monitor and region previews. */
    private final int PREVIEW_HEIGHT = 150;

    /**
     * Constructs a new editor frame for the given AGI session.
     * 
     * @param agi The target AGI session.
     */
    public SharedScreenEditorFrame(AgiPanel agiPanel) {
        this.agiPanel = agiPanel;
        this.agi = agiPanel.getAgi();
        this.screensToolkit = agi.getToolkit(Screens.class).orElse(null);
        
        setTitle("Live Screen Sharing - " + (agi.getNickname() != null ? agi.getNickname() : agi.getShortId()));
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        setLayout(new BorderLayout(10, 10));
        
        initComponents();
        setSize(1000, 750);
        setLocationRelativeTo(null);
    }

    /**
     * Initializes the UI components and layout.
     */
    private void initComponents() {
        // 1. Monitors Layout (Top)
        monitorsContainer = new JPanel(new FlowLayout(FlowLayout.LEFT, 20, 20));
        monitorsContainer.setBorder(BorderFactory.createTitledBorder("Available Monitors (Hardware Captures)"));
        
        // 2. Regions List (Center)
        regionsListPanel = new JPanel();
        regionsListPanel.setLayout(new BoxLayout(regionsListPanel, BoxLayout.Y_AXIS));
        JScrollPane regionsScroll = new JScrollPane(regionsListPanel);
        regionsScroll.setBorder(BorderFactory.createTitledBorder("Active Shared Regions"));
        
        // 3. Selection Controls (Toolbar)
        JButton addRegionBtn = new JButton("Select New Region to Share", new RegionSelectionIcon(18));
        addRegionBtn.addActionListener(e -> startRegionSelection());
        
        JPanel toolbar = new JPanel(new FlowLayout(FlowLayout.LEFT));
        toolbar.add(addRegionBtn);
        
        JPanel centerPanel = new JPanel(new BorderLayout());
        centerPanel.add(toolbar, BorderLayout.NORTH);
        centerPanel.add(regionsScroll, BorderLayout.CENTER);
        
        JButton closeBtn = new JButton("Close");
        closeBtn.addActionListener(e -> dispose());
        JPanel closePanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        closePanel.add(closeBtn);
        
        add(new JScrollPane(monitorsContainer), BorderLayout.NORTH);
        add(centerPanel, BorderLayout.CENTER);
        add(closePanel, BorderLayout.SOUTH);
        
        refreshMonitors();
        refreshRegionsList();
    }

    /**
     * Refreshes the list of available graphics devices and captures new thumbnails asynchronously.
     * <p>
     * This method triggers a background task for each screen to prevent EDT blocking 
     * which is critical for Wayland stability.
     * </p>
     */
    private void refreshMonitors() {
        monitorsContainer.removeAll();
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice[] devices = ge.getScreenDevices();
        
        for (int i = 0; i < devices.length; i++) {
            final int idx = i;
            GraphicsDevice gd = devices[idx];
            Rectangle bounds = gd.getDefaultConfiguration().getBounds();
            
            int thumbW = (int) (PREVIEW_HEIGHT * ((float) bounds.width / bounds.height));
            JPanel monitorPanel = new JPanel(new BorderLayout(5, 5));
            monitorPanel.setPreferredSize(new Dimension(thumbW + 30, PREVIEW_HEIGHT + 85));
            
            JPanel screenShape = new JPanel() {
                @Override
                protected void paintComponent(Graphics g) {
                    super.paintComponent(g);
                    Graphics2D g2 = (Graphics2D) g.create();
                    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                    
                    boolean isShared = screensToolkit != null && screensToolkit.getSharedDeviceIndexes().contains(idx);
                    
                    // 1. Outer Frame
                    g2.setColor(isShared ? new Color(50, 200, 120) : Color.DARK_GRAY);
                    g2.fillRoundRect(5, 5, getWidth() - 10, getHeight() - 25, 12, 12);
                    
                    // 2. Screen Area
                    g2.setColor(Color.BLACK);
                    int sx = 10, sy = 10, sw = getWidth() - 20, sh = getHeight() - 35;
                    g2.fillRect(sx, sy, sw, sh);
                    
                    Image thumb = monitorThumbnails.get(idx);
                    if (thumb != null) {
                        g2.drawImage(thumb, sx, sy, sw, sh, null);
                    } else {
                        g2.setColor(Color.WHITE);
                        g2.drawString("Loading...", sx + 10, sy + 25);
                    }
                    
                    // 3. Stand
                    g2.setColor(Color.GRAY);
                    g2.fillRect(getWidth() / 2 - 15, getHeight() - 25, 30, 15);
                    g2.fillRect(getWidth() / 2 - 40, getHeight() - 10, 80, 5);
                    
                    // 4. Highlight
                    if (isShared) {
                        g2.setColor(new Color(50, 200, 120, 180));
                        g2.setStroke(new BasicStroke(4));
                        g2.drawRoundRect(2, 2, getWidth() - 4, getHeight() - 22, 14, 14);
                    }
                    g2.dispose();
                }
            };
            
            boolean currentlyShared = screensToolkit != null && screensToolkit.getSharedDeviceIndexes().contains(idx);
            JCheckBox shareCheck = new JCheckBox("Share Screen " + idx, currentlyShared);
            shareCheck.addActionListener(e -> {
                if (screensToolkit != null) {
                    screensToolkit.toggleDeviceSharing(idx);
                    monitorPanel.repaint();
                }
            });
            
            JLabel resLabel = new JLabel(bounds.width + "x" + bounds.height + " [" + gd.getIDstring() + "]", SwingConstants.CENTER);
            resLabel.setFont(resLabel.getFont().deriveFont(10f));
            
            monitorPanel.add(screenShape, BorderLayout.CENTER);
            monitorPanel.add(shareCheck, BorderLayout.SOUTH);
            monitorPanel.add(resLabel, BorderLayout.NORTH);

            screenShape.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    shareCheck.doClick();
                }
            });
            
            monitorsContainer.add(monitorPanel);
            
            // Asynchronous Thumbnail Capture
            executeCaptureTask("Capture Screen " + idx, () -> UICapture.getSafeScreenCapture(gd), (img) -> {
                float aspectRatio = (float) bounds.width / bounds.height;
                int w = (int) (PREVIEW_HEIGHT * aspectRatio);
                Image thumb = img.getScaledInstance(w, PREVIEW_HEIGHT, Image.SCALE_SMOOTH);
                monitorThumbnails.put(idx, thumb);
                screenShape.repaint();
            });
        }
        
        monitorsContainer.revalidate();
        monitorsContainer.repaint();
    }

    /**
     * Synchronizes the region list with the toolkit state and updates thumbnails asynchronously.
     * <p>
     * For each active shared region, a background task performs the hardware capture 
     * to avoid Pipewire context errors.
     * </p>
     */
    private void refreshRegionsList() {
        regionsListPanel.removeAll();
        if (screensToolkit == null) return;

        for (Screens.SharedRegion region : screensToolkit.getSharedRegions()) {
            JPanel item = new JPanel(new FlowLayout(FlowLayout.LEFT, 15, 5));
            item.setMaximumSize(new Dimension(900, 100));
            item.setBorder(BorderFactory.createCompoundBorder(
                new EmptyBorder(5, 5, 5, 5),
                BorderFactory.createLineBorder(Color.LIGHT_GRAY)
            ));
            
            JLabel thumbLabel = new JLabel("Loading...");
            thumbLabel.setPreferredSize(new Dimension(140, 80));
            thumbLabel.setHorizontalAlignment(SwingConstants.CENTER);
            thumbLabel.setBorder(BorderFactory.createLineBorder(Color.BLACK));
            
            JPanel infoPanel = new JPanel(new GridLayout(2, 1));
            JLabel nameLabel = new JLabel(region.getName());
            nameLabel.setFont(nameLabel.getFont().deriveFont(Font.BOLD));
            infoPanel.add(nameLabel);
            
            JLabel boundsLabel = new JLabel(String.format("[%d, %d] %dx%d", 
                    region.getBounds().x, region.getBounds().y, 
                    region.getBounds().width, region.getBounds().height));
            boundsLabel.setFont(boundsLabel.getFont().deriveFont(11f));
            boundsLabel.setForeground(Color.GRAY);
            infoPanel.add(boundsLabel);
            
            JButton delBtn = new JButton(new DeleteIcon(18));
            delBtn.setToolTipText("Stop sharing this region");
            delBtn.addActionListener(e -> {
                screensToolkit.stopSharingRegion(region.getId());
                refreshRegionsList();
            });
            
            item.add(thumbLabel);
            item.add(infoPanel);
            item.add(delBtn);
            
            regionsListPanel.add(item);
            regionsListPanel.add(Box.createVerticalStrut(5));
            
            // Asynchronous Region Capture
            executeCaptureTask("Capture Region " + region.getId(), () -> UICapture.getSafeScreenCapture(region.getBounds()), (img) -> {
                Image regThumb = img.getScaledInstance(140, 80, Image.SCALE_SMOOTH);
                thumbLabel.setText("");
                thumbLabel.setIcon(new ImageIcon(regThumb));
                item.revalidate();
            });
        }
        
        regionsListPanel.revalidate();
        regionsListPanel.repaint();
    }

    /**
     * Initiates the interactive region selection process.
     */
    private void startRegionSelection() {
        Window selectionWindow = new JWindow();
        selectionWindow.setBackground(new Color(0, 0, 0, 1)); 
        
        Rectangle allScreens = new Rectangle();
        for (GraphicsDevice gd : GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices()) {
            allScreens = allScreens.union(gd.getDefaultConfiguration().getBounds());
        }
        selectionWindow.setBounds(allScreens);
        
        SelectionPanel selectionPanel = new SelectionPanel(allScreens, rect -> {
            selectionWindow.dispose();
            if (rect != null && rect.width > 20 && rect.height > 20) {
                screensToolkit.startSharingRegion(rect.x, rect.y, rect.width, rect.height, null);
                refreshRegionsList();
            }
        });
        selectionWindow.add(selectionPanel);
        selectionWindow.setVisible(true);
        selectionWindow.setAlwaysOnTop(true);
    }

    /**
     * Executes a hardware capture task on a background thread.
     * 
     * @param <T> The task result type.
     * @param name Task name.
     * @param backgroundLogic The capture logic.
     * @param onDone Callback on completion.
     */
    private <T> void executeCaptureTask(String name, Callable<T> backgroundLogic, java.util.function.Consumer<T> onDone) {
        SwingTask<T> task = new SwingTask<>(agiPanel, name, backgroundLogic, onDone, null, true);
        task.start();
    }

    /**
     * Overlay panel for interactive screen region selection.
     */
    private static class SelectionPanel extends JPanel {
        /** The starting point of the selection drag. */
        private Point start;
        /** The current selection rectangle. */
        private Rectangle currentRect;
        /** The total bounds covering all monitors. */
        private final Rectangle totalBounds;
        /** Callback for when a selection is completed. */
        private final java.util.function.Consumer<Rectangle> onSelected;

        /**
         * Constructs a new selection panel.
         * 
         * @param totalBounds All screen bounds combined.
         * @param onSelected Completion callback.
         */
        public SelectionPanel(Rectangle totalBounds, java.util.function.Consumer<Rectangle> onSelected) {
            this.totalBounds = totalBounds;
            this.onSelected = onSelected;
            setOpaque(false);
            setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));

            addMouseListener(new MouseAdapter() {
                @Override
                public void mousePressed(MouseEvent e) {
                    start = e.getPoint();
                }

                @Override
                public void mouseReleased(MouseEvent e) {
                    onSelected.accept(currentRect);
                }
            });

            addMouseMotionListener(new MouseMotionAdapter() {
                @Override
                public void mouseDragged(MouseEvent e) {
                    int x = Math.min(start.x, e.getX());
                    int y = Math.min(start.y, e.getY());
                    int w = Math.abs(start.x - e.getX());
                    int h = Math.abs(start.y - e.getY());
                    currentRect = new Rectangle(x, y, w, h);
                    repaint();
                }
            });
        }

        @Override
        protected void paintComponent(Graphics g) {
            Graphics2D g2 = (Graphics2D) g.create();
            g2.setColor(new Color(0, 0, 0, 120));
            g2.fillRect(0, 0, getWidth(), getHeight());

            if (currentRect != null) {
                g2.setComposite(AlphaComposite.Clear);
                g2.fillRect(currentRect.x, currentRect.y, currentRect.width, currentRect.height);
                g2.setComposite(AlphaComposite.SrcOver);
                g2.setColor(Color.RED);
                g2.setStroke(new BasicStroke(2));
                g2.draw(currentRect);
            }
            
            g2.setColor(Color.WHITE);
            g2.setFont(new Font("SansSerif", Font.BOLD, 32));
            for (GraphicsDevice gd : GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices()) {
                Rectangle b = gd.getDefaultConfiguration().getBounds();
                int lx = b.x - totalBounds.x;
                int ly = b.y - totalBounds.y;
                g2.drawString("Drag to select area...", lx + 100, ly + 100);
            }
            g2.dispose();
        }
    }
}
