/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.benchmarks;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.util.function.Consumer;
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
import lombok.extern.slf4j.Slf4j;

/**
 * Floating, always-on-top Swing recording control overlay displayed during benchmark test execution.
 * <p>
 * Displays an active recording timer, pulsing red status indicator, test identifier, and candidate model ID.
 * Provides two primary actions:
 * <ul>
 *   <li><b>[ ❌ Stop &amp; Cancel ]</b>: Discards the video and closes the recorder.</li>
 *   <li><b>[ 🚀 Stop &amp; Upload ]</b>: Finalizes the video, triggers YouTube publishing, and persists telemetry.</li>
 * </ul>
 *
 * @author anahata
 */
@Slf4j
public class BenchmarkRecordingOverlay extends JDialog {

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
     * Swing timer updating the elapsed time and driving the red dot pulse.
     */
    private final Timer clockTimer;

    /**
     * Mouse drag anchor point for smooth window relocation.
     */
    private Point dragAnchor;

    /**
     * Callback invoked when the user clicks 'Stop &amp; Upload'.
     */
    private final Runnable onUploadAction;

    /**
     * Callback invoked when the user clicks 'Stop &amp; Cancel'.
     */
    private final Runnable onCancelAction;

    /**
     * Constructs and initializes the floating benchmark recording overlay.
     *
     * @param testCode The benchmark test identifier code (e.g. "JAVA-ARKANOID-1").
     * @param modelId The candidate model identifier string (e.g. "gemini-3.6-flash").
     * @param onUploadAction The action executed when upload is confirmed.
     * @param onCancelAction The action executed when recording is cancelled.
     */
    public BenchmarkRecordingOverlay(String testCode, String modelId, Runnable onUploadAction, Runnable onCancelAction) {
        super();
        this.onUploadAction = onUploadAction;
        this.onCancelAction = onCancelAction;

        setUndecorated(true);
        setAlwaysOnTop(true);
        setResizable(false);
        setType(Type.UTILITY);

        initComponents(testCode, modelId);

        this.clockTimer = new Timer(1000, e -> {
            elapsedSeconds++;
            int mins = elapsedSeconds / 60;
            int secs = elapsedSeconds % 60;
            timerLabel.setText(String.format("%02d:%02d", mins, secs));
            recordingDot.togglePulse();
        });

        pack();
        positionTopRight();
        enableDraggability();
    }

    /**
     * Starts the overlay timer and displays the window on the screen.
     */
    public void start() {
        SwingUtilities.invokeLater(() -> {
            elapsedSeconds = 0;
            timerLabel.setText("00:00");
            clockTimer.start();
            setVisible(true);
        });
    }

    /**
     * Stops the timer and disposes of the overlay window.
     */
    public void stop() {
        SwingUtilities.invokeLater(() -> {
            if (clockTimer.isRunning()) {
                clockTimer.stop();
            }
            setVisible(false);
            dispose();
        });
    }

    /**
     * Builds and styles the internal Swing components with modern FlatLaf dark theme accents.
     *
     * @param testCode The test code.
     * @param modelId The candidate model ID.
     */
    private void initComponents(String testCode, String modelId) {
        JPanel root = new JPanel(new BorderLayout(12, 0));
        root.setBackground(new Color(15, 23, 42)); // Deep Slate #0f172a
        root.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(new Color(237, 187, 0), 2, true), // Barça Gold Border
                BorderFactory.createEmptyBorder(10, 16, 10, 16)
        ));

        // Left Section: Red Pulse Dot + Timer + Info
        JPanel leftPanel = new JPanel();
        leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.Y_AXIS));
        leftPanel.setOpaque(false);

        JPanel statusRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        statusRow.setOpaque(false);

        recordingDot.setPreferredSize(new Dimension(14, 14));
        statusRow.add(recordingDot);

        timerLabel.setFont(new Font(Font.MONOSPACED, Font.BOLD, 15));
        timerLabel.setForeground(new Color(248, 250, 252)); // Light Slate
        statusRow.add(timerLabel);

        JLabel testLabel = new JLabel("⚡ " + testCode);
        testLabel.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 12));
        testLabel.setForeground(new Color(237, 187, 0)); // Barça Gold
        statusRow.add(testLabel);

        leftPanel.add(statusRow);
        leftPanel.add(Box.createVerticalStrut(3));

        JLabel modelLabel = new JLabel("Model: " + modelId);
        modelLabel.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 11));
        modelLabel.setForeground(new Color(148, 163, 184)); // Muted slate
        leftPanel.add(modelLabel);

        root.add(leftPanel, BorderLayout.CENTER);

        // Right Section: Action Buttons
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 8, 0));
        buttonPanel.setOpaque(false);

        JButton cancelButton = new JButton("❌ Cancel");
        cancelButton.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 12));
        cancelButton.setBackground(new Color(239, 68, 68)); // Red #ef4444
        cancelButton.setForeground(Color.WHITE);
        cancelButton.setFocusPainted(false);
        cancelButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        cancelButton.addActionListener(e -> {
            stop();
            if (onCancelAction != null) {
                onCancelAction.run();
            }
        });

        JButton uploadButton = new JButton("🚀 Stop & Upload");
        uploadButton.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 12));
        uploadButton.setBackground(new Color(34, 197, 94)); // Emerald Green #22c55e
        uploadButton.setForeground(Color.BLACK);
        uploadButton.setFocusPainted(false);
        uploadButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        uploadButton.addActionListener(e -> {
            stop();
            if (onUploadAction != null) {
                onUploadAction.run();
            }
        });

        buttonPanel.add(cancelButton);
        buttonPanel.add(uploadButton);

        root.add(buttonPanel, BorderLayout.EAST);
        setContentPane(root);
    }

    /**
     * Positions the overlay at the top-right corner of the primary display screen.
     */
    private void positionTopRight() {
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        int margin = 24;
        int x = (int) screenSize.getWidth() - getWidth() - margin;
        int y = margin;
        setLocation(x, y);
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
