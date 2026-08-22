/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.screenrecording;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Robot;
import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

/**
 * Universal, floating, always-on-top Swing screen recording control overlay.
 * <p>
 * Provides interactive pre-launch multi-monitor selection with real-time thumbnail snapshots,
 * active recording timer with pulsing red indicator, live 1-second interval mini-preview,
 * and a three-action control bar:
 * </p>
 * <ul>
 *   <li><b>[ ❌ Cancel ]</b>: Discards the video and closes the recorder.</li>
 *   <li><b>[ 💾 Save ]</b>: Finalizes the video locally without external publishing.</li>
 *   <li><b>[ 🚀 Save &amp; Upload ]</b>: Finalizes the video, executes upload publishing, and records telemetry.</li>
 * </ul>
 *
 * @author anahata
 */
@Slf4j
public class ScreenRecordingOverlay extends JDialog {

    /**
     * The pulsing red recording dot indicator component.
     */
    private final RecordingDot recordingDot = new RecordingDot();

    /**
     * The label displaying elapsed recording time in {@code MM:SS} format.
     */
    private final JLabel timerLabel = new JLabel("00:00");

    /**
     * Elapsed seconds counter.
     */
    private int elapsedSeconds = 0;

    /**
     * The label displaying live 1-second interval screen preview during active recording.
     */
    private final JLabel livePreviewLabel = new JLabel();

    /**
     * Cache for monitor pre-launch thumbnail snapshots.
     */
    private final Map<Integer, Image> monitorThumbnails = new HashMap<>();

    /**
     * Swing timer updating the elapsed time, red dot pulse, and live screen preview.
     */
    private final Timer clockTimer;

    /**
     * Mouse drag anchor point for smooth window relocation.
     */
    private Point dragAnchor;

    /**
     * The selected screen graphics device index.
     */
    @Getter
    @Setter
    private int selectedDeviceIndex = 0;

    /**
     * The title displayed on the overlay header.
     */
    @Getter
    @Setter
    private String headerTitle;

    /**
     * The subtitle displayed on the overlay header.
     */
    @Getter
    @Setter
    private String headerSubtitle;

    /**
     * The custom label for the start recording button.
     */
    @Getter
    @Setter
    private String startRecordingLabel = "▶ Start Recording";

    /**
     * The custom label for the save locally button.
     */
    @Getter
    @Setter
    private String saveLocalLabel = "💾 Save";

    /**
     * The custom label for the stop and upload button.
     */
    @Getter
    @Setter
    private String saveAndUploadLabel = "🚀 Save & Upload";

    /**
     * The custom label for the cancel button.
     */
    @Getter
    @Setter
    private String cancelLabel = "❌ Cancel";

    /**
     * Callback invoked when the user clicks 'Start Recording'.
     */
    private final Runnable onStartAction;

    /**
     * Callback invoked when the user clicks 'Save' (local persistence only).
     */
    private final Runnable onSaveLocalAction;

    /**
     * Callback invoked when the user clicks 'Save &amp; Upload'.
     */
    private final Runnable onUploadAction;

    /**
     * Callback invoked when the user clicks 'Cancel'.
     */
    private final Runnable onCancelAction;

    /**
     * The root content panel.
     */
    private JPanel rootPanel;

    /**
     * Constructs the universal floating recording overlay.
     *
     * @param headerTitle The primary title string (e.g. "JAVA-ARKANOID-1" or "Screen Recording").
     * @param headerSubtitle The subtitle string (e.g. candidate model ID or descriptor).
     * @param onStartAction The action executed when start is clicked.
     * @param onSaveLocalAction The action executed when save local is clicked.
     * @param onUploadAction The action executed when save and upload is clicked.
     * @param onCancelAction The action executed when recording is cancelled.
     */
    public ScreenRecordingOverlay(String headerTitle, String headerSubtitle, Runnable onStartAction, Runnable onSaveLocalAction, Runnable onUploadAction, Runnable onCancelAction) {
        super();
        this.headerTitle = headerTitle;
        this.headerSubtitle = headerSubtitle;
        this.onStartAction = onStartAction;
        this.onSaveLocalAction = onSaveLocalAction;
        this.onUploadAction = onUploadAction;
        this.onCancelAction = onCancelAction;

        setUndecorated(true);
        setAlwaysOnTop(true);
        setResizable(false);
        setType(Type.UTILITY);

        this.clockTimer = new Timer(1000, e -> {
            elapsedSeconds++;
            int mins = elapsedSeconds / 60;
            int secs = elapsedSeconds % 60;
            timerLabel.setText(String.format("%02d:%02d", mins, secs));
            recordingDot.togglePulse();

            // Refresh live screen preview every second in a background thread
            updateLivePreviewAsync();
        });

        // Initialize default screen device based on current mouse location
        try {
            Point mouseLoc = java.awt.MouseInfo.getPointerInfo().getLocation();
            GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
            GraphicsDevice[] devices = ge.getScreenDevices();
            for (int i = 0; i < devices.length; i++) {
                if (devices[i].getDefaultConfiguration().getBounds().contains(mouseLoc)) {
                    selectedDeviceIndex = i;
                    break;
                }
            }
        } catch (Exception ignored) {
        }

        initPreLaunchUI();
        pack();
        positionTopRight();
        enableDraggability();
    }

    /**
     * Captures a live downscaled screenshot of the currently recorded screen asynchronously.
     */
    private void updateLivePreviewAsync() {
        new Thread(() -> {
            try {
                GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
                GraphicsDevice[] devices = ge.getScreenDevices();
                if (selectedDeviceIndex >= 0 && selectedDeviceIndex < devices.length) {
                    Rectangle bounds = devices[selectedDeviceIndex].getDefaultConfiguration().getBounds();
                    Robot robot = new Robot();
                    BufferedImage capture = robot.createScreenCapture(bounds);

                    int targetH = 34;
                    int targetW = (int) (targetH * ((float) bounds.width / bounds.height));
                    Image scaled = capture.getScaledInstance(targetW, targetH, Image.SCALE_SMOOTH);

                    SwingUtilities.invokeLater(() -> {
                        livePreviewLabel.setIcon(new javax.swing.ImageIcon(scaled));
                    });
                }
            } catch (Exception ignored) {
            }
        }, "ScreenRecording-LivePreview-Capture").start();
    }

    /**
     * Configures custom button labels for the action buttons.
     *
     * @param startLabel Custom text for start button.
     * @param saveLocalLabel Custom text for save local button.
     * @param uploadLabel Custom text for save and upload button.
     * @param cancelLabel Custom text for cancel button.
     * @return This overlay instance.
     */
    public ScreenRecordingOverlay withCustomLabels(String startLabel, String saveLocalLabel, String uploadLabel, String cancelLabel) {
        if (startLabel != null && !startLabel.isBlank()) this.startRecordingLabel = startLabel;
        if (saveLocalLabel != null && !saveLocalLabel.isBlank()) this.saveLocalLabel = saveLocalLabel;
        if (uploadLabel != null && !uploadLabel.isBlank()) this.saveAndUploadLabel = uploadLabel;
        if (cancelLabel != null && !cancelLabel.isBlank()) this.cancelLabel = cancelLabel;
        initPreLaunchUI();
        pack();
        return this;
    }

    /**
     * Displays the overlay in pre-launch mode ready for user to click Start.
     */
    public void showPreLaunch() {
        SwingUtilities.invokeLater(() -> {
            initPreLaunchUI();
            pack();
            setVisible(true);
        });
    }

    /**
     * Transitions the overlay from pre-launch to active recording view.
     */
    public void transitionToActiveRecording() {
        SwingUtilities.invokeLater(() -> {
            initActiveRecordingUI();
            pack();
            elapsedSeconds = 0;
            timerLabel.setText("00:00");
            clockTimer.start();
        });
    }

    /**
     * Stops the timer and disposes of the overlay window.
     */
    public void stop() {
        SwingUtilities.invokeLater(() -> {
            if (clockTimer != null && clockTimer.isRunning()) {
                clockTimer.stop();
            }
            setVisible(false);
            dispose();
        });
    }

    /**
     * Builds the Pre-Launch UI showing test/session info, visual monitor cards, and the Start Recording button.
     */
    private void initPreLaunchUI() {
        if (rootPanel == null) {
            rootPanel = new JPanel(new BorderLayout(14, 0));
            rootPanel.setBackground(new Color(15, 23, 42)); // Deep Slate #0f172a
            rootPanel.setBorder(BorderFactory.createCompoundBorder(
                    BorderFactory.createLineBorder(new Color(237, 187, 0), 2, true), // Barça Gold
                    BorderFactory.createEmptyBorder(10, 16, 10, 16)
            ));
            setContentPane(rootPanel);
        } else {
            rootPanel.removeAll();
        }

        // Info Panel
        JPanel leftPanel = new JPanel();
        leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.Y_AXIS));
        leftPanel.setOpaque(false);

        JLabel titleLbl = new JLabel(headerTitle != null ? headerTitle : "Screen Recording");
        titleLbl.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 13));
        titleLbl.setForeground(new Color(237, 187, 0)); // Barça Gold
        leftPanel.add(titleLbl);

        if (headerSubtitle != null && !headerSubtitle.isBlank()) {
            leftPanel.add(Box.createVerticalStrut(3));
            JLabel subLbl = new JLabel(headerSubtitle);
            subLbl.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 11));
            subLbl.setForeground(new Color(148, 163, 184));
            leftPanel.add(subLbl);
        }

        rootPanel.add(leftPanel, BorderLayout.WEST);

        // Center: Visual Monitors Selector Container
        JPanel centerMonitorsPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 8, 0));
        centerMonitorsPanel.setOpaque(false);

        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice[] devices = ge.getScreenDevices();

        List<Integer> sortedIndices = IntStream.range(0, devices.length)
                .boxed()
                .sorted(java.util.Comparator.comparingInt(i -> devices[i].getDefaultConfiguration().getBounds().x))
                .toList();

        for (int idx : sortedIndices) {
            GraphicsDevice gd = devices[idx];
            Rectangle bounds = gd.getDefaultConfiguration().getBounds();
            int cardH = 44;
            int cardW = (int) (cardH * ((float) bounds.width / bounds.height)) + 12;

            JPanel monitorCard = new JPanel(new BorderLayout()) {
                @Override
                protected void paintComponent(Graphics g) {
                    super.paintComponent(g);
                    Graphics2D g2 = (Graphics2D) g.create();
                    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                    boolean isSelected = (idx == selectedDeviceIndex);
                    g2.setColor(isSelected ? new Color(237, 187, 0) : new Color(51, 65, 85));
                    g2.fillRoundRect(0, 0, getWidth(), getHeight(), 8, 8);

                    // Screen background / thumbnail area
                    g2.setColor(Color.BLACK);
                    g2.fillRoundRect(2, 2, getWidth() - 4, getHeight() - 14, 6, 6);

                    Image thumb = monitorThumbnails.get(idx);
                    if (thumb != null) {
                        g2.drawImage(thumb, 2, 2, getWidth() - 4, getHeight() - 14, null);
                    } else {
                        g2.setColor(new Color(148, 163, 184));
                        g2.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 9));
                        g2.drawString("Screen " + idx, 6, 16);
                    }

                    // Stand / Base Label
                    g2.setColor(isSelected ? Color.BLACK : Color.WHITE);
                    g2.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 9));
                    String label = "Screen " + idx;
                    int strW = g2.getFontMetrics().stringWidth(label);
                    g2.drawString(label, (getWidth() - strW) / 2, getHeight() - 3);

                    if (isSelected) {
                        g2.setColor(new Color(237, 187, 0));
                        g2.setStroke(new java.awt.BasicStroke(2));
                        g2.drawRoundRect(0, 0, getWidth() - 1, getHeight() - 1, 8, 8);
                    }
                    g2.dispose();
                }
            };

            monitorCard.setPreferredSize(new Dimension(cardW, cardH));
            monitorCard.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            monitorCard.setToolTipText("Select Screen " + idx + " (" + bounds.width + "x" + bounds.height + " " + gd.getIDstring() + ")");

            monitorCard.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    selectedDeviceIndex = idx;
                    positionOnScreen(selectedDeviceIndex);
                    centerMonitorsPanel.repaint();
                }
            });

            centerMonitorsPanel.add(monitorCard);

            // Asynchronously capture thumbnail for this monitor card
            if (!monitorThumbnails.containsKey(idx)) {
                new Thread(() -> {
                    try {
                        Robot robot = new Robot();
                        BufferedImage img = robot.createScreenCapture(bounds);
                        Image thumb = img.getScaledInstance(cardW, cardH - 12, Image.SCALE_SMOOTH);
                        monitorThumbnails.put(idx, thumb);
                        SwingUtilities.invokeLater(centerMonitorsPanel::repaint);
                    } catch (Exception ignored) {
                    }
                }, "ScreenRecording-PreLaunchThumb-" + idx).start();
            }
        }

        rootPanel.add(centerMonitorsPanel, BorderLayout.CENTER);

        // Buttons Panel
        JPanel rightControlPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 8, 0));
        rightControlPanel.setOpaque(false);

        JButton cancelBtn = new JButton(cancelLabel);
        cancelBtn.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 12));
        cancelBtn.setBackground(new Color(239, 68, 68));
        cancelBtn.setForeground(Color.WHITE);
        cancelBtn.setFocusPainted(false);
        cancelBtn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        cancelBtn.addActionListener(e -> {
            stop();
            if (onCancelAction != null) onCancelAction.run();
        });

        JButton startBtn = new JButton(startRecordingLabel);
        startBtn.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 12));
        startBtn.setBackground(new Color(34, 197, 94)); // Green
        startBtn.setForeground(Color.BLACK);
        startBtn.setFocusPainted(false);
        startBtn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        startBtn.addActionListener(e -> {
            transitionToActiveRecording();
            if (onStartAction != null) {
                new Thread(onStartAction, "ScreenRecording-Launcher").start();
            }
        });

        rightControlPanel.add(cancelBtn);
        rightControlPanel.add(startBtn);
        rootPanel.add(rightControlPanel, BorderLayout.EAST);
        rootPanel.revalidate();
        rootPanel.repaint();
    }

    /**
     * Builds the Active Recording UI showing the live timer, pulsing dot, live 1s mini preview, and action buttons.
     */
    private void initActiveRecordingUI() {
        rootPanel.removeAll();

        // Left: Pulse Dot + Timer + Info
        JPanel leftPanel = new JPanel();
        leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.Y_AXIS));
        leftPanel.setOpaque(false);

        JPanel statusRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        statusRow.setOpaque(false);

        recordingDot.setPreferredSize(new Dimension(14, 14));
        statusRow.add(recordingDot);

        timerLabel.setFont(new Font(Font.MONOSPACED, Font.BOLD, 15));
        timerLabel.setForeground(new Color(248, 250, 252));
        statusRow.add(timerLabel);

        JLabel titleLbl = new JLabel("⚡ " + headerTitle);
        titleLbl.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 12));
        titleLbl.setForeground(new Color(237, 187, 0));
        statusRow.add(titleLbl);

        leftPanel.add(statusRow);
        leftPanel.add(Box.createVerticalStrut(3));

        JLabel infoLbl = new JLabel((headerSubtitle != null ? headerSubtitle : "") + " (Screen " + selectedDeviceIndex + ")");
        infoLbl.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 11));
        infoLbl.setForeground(new Color(148, 163, 184));
        leftPanel.add(infoLbl);

        rootPanel.add(leftPanel, BorderLayout.WEST);

        // Center: Live 1s Screen Preview Mini Display
        JPanel centerPreviewPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 0, 0));
        centerPreviewPanel.setOpaque(false);
        livePreviewLabel.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(new Color(237, 187, 0, 180), 1, true),
                BorderFactory.createEmptyBorder(1, 1, 1, 1)
        ));
        livePreviewLabel.setToolTipText("Live Screen " + selectedDeviceIndex + " Recording Preview (Updated every 1s)");
        centerPreviewPanel.add(livePreviewLabel);
        rootPanel.add(centerPreviewPanel, BorderLayout.CENTER);

        // Right Section: Cancel, Save Local & Save & Upload
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 8, 0));
        buttonPanel.setOpaque(false);

        JButton cancelBtn = new JButton(cancelLabel);
        cancelBtn.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 12));
        cancelBtn.setBackground(new Color(239, 68, 68));
        cancelBtn.setForeground(Color.WHITE);
        cancelBtn.setFocusPainted(false);
        cancelBtn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        cancelBtn.addActionListener(e -> {
            stop();
            if (onCancelAction != null) onCancelAction.run();
        });

        JButton saveLocalBtn = new JButton(saveLocalLabel);
        saveLocalBtn.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 12));
        saveLocalBtn.setBackground(new Color(59, 130, 246)); // Blue #3b82f6
        saveLocalBtn.setForeground(Color.WHITE);
        saveLocalBtn.setFocusPainted(false);
        saveLocalBtn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        saveLocalBtn.addActionListener(e -> {
            stop();
            if (onSaveLocalAction != null) onSaveLocalAction.run();
        });

        JButton uploadBtn = new JButton(saveAndUploadLabel);
        uploadBtn.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 12));
        uploadBtn.setBackground(new Color(34, 197, 94));
        uploadBtn.setForeground(Color.BLACK);
        uploadBtn.setFocusPainted(false);
        uploadBtn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        uploadBtn.addActionListener(e -> {
            stop();
            if (onUploadAction != null) onUploadAction.run();
        });

        buttonPanel.add(cancelBtn);
        buttonPanel.add(saveLocalBtn);
        buttonPanel.add(uploadBtn);
        rootPanel.add(buttonPanel, BorderLayout.EAST);
        rootPanel.revalidate();
        rootPanel.repaint();
    }

    /**
     * Repositions the overlay to the top-right corner of the specified screen device.
     *
     * @param screenIndex The 0-based screen device index.
     */
    public void positionOnScreen(int screenIndex) {
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice[] devices = ge.getScreenDevices();
        if (screenIndex >= 0 && screenIndex < devices.length) {
            Rectangle bounds = devices[screenIndex].getDefaultConfiguration().getBounds();
            int margin = 24;
            int x = bounds.x + bounds.width - getWidth() - margin;
            int y = bounds.y + margin;
            setLocation(x, y);
        } else {
            Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
            int margin = 24;
            int x = (int) screenSize.getWidth() - getWidth() - margin;
            int y = margin;
            setLocation(x, y);
        }
    }

    /**
     * Positions the overlay at the top-right corner of the primary display screen.
     */
    private void positionTopRight() {
        positionOnScreen(selectedDeviceIndex);
    }

    /**
     * Enables mouse dragging to relocate the overlay anywhere on screen.
     */
    private void enableDraggability() {
        addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                dragAnchor = e.getPoint();
            }
        });

        addMouseMotionListener(new MouseMotionAdapter() {
            @Override
            public void mouseDragged(MouseEvent e) {
                Point current = getLocation();
                setLocation(current.x + e.getX() - dragAnchor.x, current.y + e.getY() - dragAnchor.y);
            }
        });
    }

    /**
     * Custom painting component rendering a pulsing red recording dot.
     */
    private static class RecordingDot extends JComponent {

        /**
         * State controlling the pulsing opacity toggle.
         */
        private boolean activePulse = true;

        /**
         * Default constructor for the recording dot component.
         */
        public RecordingDot() {
        }

        /**
         * Toggles the pulse state and triggers repaint.
         */
        public void togglePulse() {
            activePulse = !activePulse;
            repaint();
        }

        @Override
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            Graphics2D g2 = (Graphics2D) g.create();
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            if (activePulse) {
                g2.setColor(new Color(239, 68, 68, 220)); // Bright Red
            } else {
                g2.setColor(new Color(185, 28, 28, 140)); // Darker Red
            }

            int diameter = Math.min(getWidth(), getHeight()) - 2;
            int x = (getWidth() - diameter) / 2;
            int y = (getHeight() - diameter) / 2;
            g2.fillOval(x, y, diameter, diameter);
            g2.dispose();
        }
    }
}
