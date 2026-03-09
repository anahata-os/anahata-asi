/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context.gc;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Arc2D;
import java.text.NumberFormat;
import java.time.Instant;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.table.DefaultTableModel;
import lombok.NonNull;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXTable;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.context.ContextManager;
import uno.anahata.asi.agi.context.ContextWindowGarbageCollector;
import uno.anahata.asi.agi.context.GarbageCollectorRecord;
import uno.anahata.asi.internal.TimeUtils;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.RestartIcon;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingTask;

/**
 * A specialized panel for monitoring the Context Window Garbage Collection (CwGC) 
 * status, showing token recycling metrics and pruned content overhead with 
 * a high-fidelity donut chart and a recycling log.
 * <p>
 * This panel is reactive: it listens to history and resource changes to update 
 * the metabolism chart automatically using a background {@link SwingTask}.
 * </p>
 * 
 * @author anahata
 */
public class CwGcPanel extends JPanel {

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance();

    private final AgiPanel agiPanel;
    private Agi agi;

    private JLabel systemTokensLabel;
    private JLabel toolTokensLabel;
    private JLabel metadataTokensLabel;
    private JLabel activeHistoryTokensLabel;
    private JLabel prunedHistoryTokensLabel;
    private JLabel ragTokensLabel;
    private JLabel totalPromptLoadLabel;
    
    private JButton refreshBtn;
    private JXTable logTable;
    private DefaultTableModel logModel;
    private MetabolicDonutChart donutChart;

    /** Reactive listener for history changes. */
    private EdtPropertyChangeListener historyListener;
    /** Reactive listener for resource changes. */
    private EdtPropertyChangeListener resourcesListener;

    /**
     * Constructs a new CwGcPanel.
     * @param agiPanel The parent agi panel.
     */
    public CwGcPanel(@NonNull AgiPanel agiPanel) {
        setLayout(new BorderLayout());
        this.agiPanel = agiPanel;
        this.agi = agiPanel.getAgi();
        
        initComponents();
        setupListeners();
    }

    /**
     * Sets up the reactive listeners for history and resources.
     */
    private void setupListeners() {
        if (historyListener != null) {
            historyListener.unbind();
        }
        if (resourcesListener != null) {
            resourcesListener.unbind();
        }
        
        this.historyListener = new EdtPropertyChangeListener(this, agi.getContextManager(), "history", evt -> refresh());
        this.resourcesListener = new EdtPropertyChangeListener(this, agi.getResourceManager2(), "resources", evt -> refresh());
    }

    private void initComponents() {
        JPanel mainContent = new JPanel(new MigLayout("insets 10, fill", "[grow]", "[350!][grow]"));

        // --- Top: Metrics and Chart ---
        JPanel topPanel = new JPanel(new MigLayout("insets 0, fill, gap 20", "[350!][grow]", "[grow]"));
        topPanel.setBorder(BorderFactory.createTitledBorder("CwGC Metabolic Status"));

        // Left: Detailed Metrics
        JPanel metricsPanel = new JPanel(new MigLayout("insets 10, fillx, gap 5", "[right][left, grow]", "[]5[]5[]5[]5[]5[]15[]20[]"));
        Font valueFont = new JLabel().getFont().deriveFont(Font.BOLD, 14f);

        metricsPanel.add(new JLabel("System Instructions:"));
        systemTokensLabel = createValueLabel(valueFont, new Color(52, 152, 219)); // Blue
        metricsPanel.add(systemTokensLabel, "wrap");

        metricsPanel.add(new JLabel("Tool Declarations:"));
        toolTokensLabel = createValueLabel(valueFont, new Color(155, 89, 182)); // Purple
        metricsPanel.add(toolTokensLabel, "wrap");

        metricsPanel.add(new JLabel("Metadata Overhead:"));
        metadataTokensLabel = createValueLabel(valueFont, new Color(243, 156, 18)); // Orange
        metricsPanel.add(metadataTokensLabel, "wrap");

        metricsPanel.add(new JLabel("Active History:"));
        activeHistoryTokensLabel = createValueLabel(valueFont, new Color(46, 204, 113)); // Green
        metricsPanel.add(activeHistoryTokensLabel, "wrap");

        metricsPanel.add(new JLabel("RAG Context:"));
        ragTokensLabel = createValueLabel(valueFont, new Color(26, 188, 156)); // Turquoise
        metricsPanel.add(ragTokensLabel, "wrap");

        metricsPanel.add(new JLabel("Pruned (Recycled):"));
        prunedHistoryTokensLabel = createValueLabel(valueFont, new Color(149, 165, 166)); // Grey
        metricsPanel.add(prunedHistoryTokensLabel, "wrap");
        
        metricsPanel.add(new JLabel("Total Prompt Load:"), "gaptop 10");
        totalPromptLoadLabel = new JLabel("0");
        totalPromptLoadLabel.setFont(valueFont.deriveFont(18f));
        metricsPanel.add(totalPromptLoadLabel, "wrap");

        refreshBtn = new JButton("Refresh Now", new RestartIcon(16));
        refreshBtn.addActionListener(e -> refresh());
        metricsPanel.add(refreshBtn, "span 2, gaptop 10");

        topPanel.add(metricsPanel, "top");

        // Right: Visualization
        donutChart = new MetabolicDonutChart();
        topPanel.add(donutChart, "grow");

        mainContent.add(topPanel, "growx, wrap");

        // --- Bottom: Recycling Log ---
        JPanel logPanel = new JPanel(new BorderLayout());
        logPanel.setBorder(BorderFactory.createTitledBorder("Recycling Log (Hard Pruning Events)"));

        logModel = new DefaultTableModel(new Object[]{"Timestamp", "Msg ID", "Type", "Tokens Recycled"}, 0) {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };
        logTable = new JXTable(logModel);
        logTable.setColumnControlVisible(true);
        logPanel.add(new JScrollPane(logTable), BorderLayout.CENTER);

        JPanel logActions = new JPanel(new MigLayout("insets 5", "[grow][]"));
        JButton clearBtn = new JButton("Clear GC Logs", new DeleteIcon(16));
        clearBtn.addActionListener(e -> {
            agi.getContextManager().getGarbageCollector().clearLog();
            refreshLogTable();
        });
        logActions.add(new JLabel("Turn recycling occurs when a message is fully pruned."), "growx");
        logActions.add(clearBtn);
        logPanel.add(logActions, BorderLayout.SOUTH);

        mainContent.add(logPanel, "grow");

        add(mainContent, BorderLayout.CENTER);
        
        JPanel infoPanel = new JPanel(new BorderLayout());
        infoPanel.setBorder(BorderFactory.createEmptyBorder(0, 15, 10, 15));
        JLabel info = new JLabel("<html><b>Metabolism Engine:</b> The <font color='#2ecc71'><b>Active Content</b></font> is the direct context window payload. "
                + "Pruned turns are stored as compressed hints in metadata.</html>");
        infoPanel.add(info, BorderLayout.NORTH);
        add(infoPanel, BorderLayout.SOUTH);
    }

    private JLabel createValueLabel(Font font, Color color) {
        JLabel label = new JLabel("0");
        label.setFont(font);
        label.setForeground(color);
        return label;
    }

    /**
     * Reloads the panel with the new agi session state.
     */
    public void reload() {
        this.agi = agiPanel.getAgi();
        setupListeners();
        refresh();
    }

    /**
     * Performs a high-fidelity metabolism refresh using a background {@link SwingTask}.
     * <p>The refresh button is disabled and updated with feedback during the process.</p>
     */
    public void refresh() {
        if (refreshBtn != null) {
            refreshBtn.setEnabled(false);
            refreshBtn.setText("Refreshing...");
        }

        ContextManager cm = agi.getContextManager();
        ContextWindowGarbageCollector gc = cm.getGarbageCollector();
        
        new SwingTask<ContextWindowGarbageCollector.Stats>(this, "CwGC Metabolism Check", () -> {
            gc.calculate();
            return gc.getStats();
        }, stats -> {
            int threshold = cm.getTokenThreshold();

            systemTokensLabel.setText(NUMBER_FORMAT.format(stats.getSystemInstructionsTokens()));
            toolTokensLabel.setText(NUMBER_FORMAT.format(stats.getToolDeclarationsTokens()));
            metadataTokensLabel.setText(NUMBER_FORMAT.format(stats.getMetadataTokens()));
            activeHistoryTokensLabel.setText(NUMBER_FORMAT.format(stats.getActiveHistoryTokens()));
            prunedHistoryTokensLabel.setText(NUMBER_FORMAT.format(stats.getPrunedHistoryTokens()));
            ragTokensLabel.setText(NUMBER_FORMAT.format(stats.getRagTokens()));
            
            totalPromptLoadLabel.setText(NUMBER_FORMAT.format(stats.getTotalPromptLoad()) + " / " + NUMBER_FORMAT.format(threshold));
            
            donutChart.update(stats, threshold);
            refreshLogTable();
            
            if (refreshBtn != null) {
                refreshBtn.setEnabled(true);
                refreshBtn.setText("Refresh Now");
            }
        }, error -> {
            if (refreshBtn != null) {
                refreshBtn.setEnabled(true);
                refreshBtn.setText("Refresh Now");
            }
        }).execute();
    }

    private void refreshLogTable() {
        List<GarbageCollectorRecord> records = agi.getContextManager().getGarbageCollector().getRecords();
        if (records.size() == logModel.getRowCount()) {
            return; // No new records
        }
        
        logModel.setRowCount(0);
        for (GarbageCollectorRecord record : records) {
            logModel.addRow(new Object[]{
                TimeUtils.formatSmartTimestamp(Instant.ofEpochMilli(record.getTimestamp())),
                record.getMessageId(),
                record.getType(),
                NUMBER_FORMAT.format(record.getTokenCount())
            });
        }
    }

    /**
     * A high-fidelity donut chart visualizing the categorized token distribution.
     */
    private static class MetabolicDonutChart extends JComponent {
        private ContextWindowGarbageCollector.Stats stats;
        private int threshold;

        public void update(ContextWindowGarbageCollector.Stats stats, int threshold) {
            this.stats = stats;
            this.threshold = threshold;
            repaint();
        }

        @Override
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            if (stats == null) {
                return;
            }

            Graphics2D g2d = (Graphics2D) g.create();
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            int size = Math.min(getWidth(), getHeight()) - 60;
            int x = (getWidth() - size) / 2;
            int y = (getHeight() - size) / 2;
            int stroke = 50;

            // Background Ring
            g2d.setStroke(new BasicStroke(stroke, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND));
            g2d.setColor(new Color(236, 240, 241));
            g2d.drawOval(x + stroke/2, y + stroke/2, size - stroke, size - stroke);

            double total = stats.getTotalPromptLoad();
            if (total == 0) {
                total = 1;
            }

            double currentAngle = 90;

            // 1. System (Blue)
            currentAngle = drawArc(g2d, x, y, size, stroke, currentAngle, stats.getSystemInstructionsTokens() / total, new Color(52, 152, 219));
            // 2. Tools (Purple)
            currentAngle = drawArc(g2d, x, y, size, stroke, currentAngle, stats.getToolDeclarationsTokens() / total, new Color(155, 89, 182));
            // 3. Metadata (Orange)
            currentAngle = drawArc(g2d, x, y, size, stroke, currentAngle, stats.getMetadataTokens() / total, new Color(243, 156, 18));
            // 4. History (Green)
            currentAngle = drawArc(g2d, x, y, size, stroke, currentAngle, stats.getActiveHistoryTokens() / total, new Color(46, 204, 113));
            // 5. RAG (Turquoise)
            currentAngle = drawArc(g2d, x, y, size, stroke, currentAngle, stats.getRagTokens() / total, new Color(26, 188, 156));

            // Inner Ring: Pruned (Grey)
            if (stats.getPrunedHistoryTokens() > 0) {
                g2d.setStroke(new BasicStroke(15, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND));
                g2d.setColor(new Color(189, 195, 199));
                g2d.drawOval(x + stroke + 15, y + stroke + 15, size - (stroke*2) - 30, size - (stroke*2) - 30);
            }

            // Center Text
            double usagePct = Math.min(1.0, total / threshold);
            g2d.setFont(getFont().deriveFont(Font.BOLD, 28f));
            String usageText = (int)(usagePct * 100) + "%";
            int tw = g2d.getFontMetrics().stringWidth(usageText);
            g2d.setColor(usagePct > 0.9 ? new Color(192, 57, 43) : Color.DARK_GRAY);
            g2d.drawString(usageText, getWidth()/2 - tw/2, getHeight()/2 + 10);
            
            g2d.setFont(getFont().deriveFont(12f));
            g2d.setColor(Color.GRAY);
            String subText = "CAPACITY";
            int stw = g2d.getFontMetrics().stringWidth(subText);
            g2d.drawString(subText, getWidth()/2 - stw/2, getHeight()/2 + 30);

            g2d.dispose();
        }

        private double drawArc(Graphics2D g2d, int x, int y, int size, int stroke, double startAngle, double pct, Color color) {
            if (pct <= 0) {
                return startAngle;
            }
            double angle = pct * 360.0;
            g2d.setColor(color);
            g2d.draw(new Arc2D.Double(x + stroke/2, y + stroke/2, size - stroke, size - stroke, startAngle, -angle, Arc2D.OPEN));
            return startAngle - angle;
        }
    }

    /** {@inheritDoc} 
     * Initial refresh on display.
     */
    @Override
    public void addNotify() {
        super.addNotify();
        refresh();
    }
}
