/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context.gc;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.text.NumberFormat;
import java.time.Instant;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
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
    private JLabel strategyLabel;

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
        
        this.historyListener = new EdtPropertyChangeListener(this, agi.getContextManager(), "history", evt -> {
            if (isShowing()) {
                refresh();
            }
        });
        this.resourcesListener = new EdtPropertyChangeListener(this, agi.getResourceManager(), "resources", evt -> {
            if (isShowing()) {
                refresh();
            }
        });
    }

    /**
     * Constructs and organizes the complex UI tree for the metabolism dashboard.
     * This method uses {@link MigLayout} to create a responsive, high-fidelity grid
     * that balances the detailed metric labels with the metabolic donut chart 
     * and the historical recycling log.
     */
    private void initComponents() {
        JPanel headerPanel = new JPanel(new MigLayout("insets 10, fillx", "[grow]", "[]0[]"));
        headerPanel.setBackground(new Color(245, 245, 245));
        headerPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.LIGHT_GRAY));
        JLabel titleLabel = new JLabel("Context Window Garbage Collector");
        titleLabel.setFont(new Font("SansSerif", Font.BOLD, 18));
        headerPanel.add(titleLabel, "wrap");

        strategyLabel = new JLabel();
        strategyLabel.setFont(new Font("SansSerif", Font.PLAIN, 14));
        headerPanel.add(strategyLabel);

        add(headerPanel, BorderLayout.NORTH);

        JPanel mainContent = new JPanel(new MigLayout("insets 0 5 0 5, fill, gapy 0", "[grow]", "[]0[]0[grow]"));

        JLabel engineLabel = new JLabel("<html><b>Metabolism Engine:</b> A hint about all <i>Effectively Pruned</i> parts is included in the prompt. "
                + "Message garbage collection occurs when all parts in a message are <i>Effectively Pruned</i>.</html>");
        engineLabel.setFont(new Font("SansSerif", Font.PLAIN, 12));
        engineLabel.setBorder(BorderFactory.createEmptyBorder(2, 5, 2, 5));
        mainContent.add(engineLabel, "wrap");

        // --- Top: Metrics and Chart ---
        JPanel topPanel = new JPanel(new MigLayout("insets 0, fill, gap 20", "[350!][grow]", "[grow]"));
        topPanel.setBorder(BorderFactory.createTitledBorder("CwGC Metabolic Status"));

        // Left: Detailed Metrics
        JPanel metricsPanel = new JPanel(new MigLayout("insets 10, fillx, gap 5", "[right][left, grow]", "[]5[]5[]5[]5[]5[]5[]5[]"));
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

        metricsPanel.add(new JLabel("Effectively Pruned:"));
        prunedHistoryTokensLabel = createValueLabel(valueFont, new Color(149, 165, 166)); // Grey
        metricsPanel.add(prunedHistoryTokensLabel, "wrap");
        
        metricsPanel.add(new JLabel("Total Prompt Load:"), "gaptop 5");
        totalPromptLoadLabel = new JLabel("0");
        totalPromptLoadLabel.setFont(valueFont.deriveFont(18f));
        metricsPanel.add(totalPromptLoadLabel, "wrap");

        refreshBtn = new JButton("Refresh Now", new RestartIcon(16));
        refreshBtn.addActionListener(e -> refresh());
        metricsPanel.add(refreshBtn, "span 2, gaptop 5");

        topPanel.add(metricsPanel, "top");

        // Right: Visualization
        donutChart = new MetabolicDonutChart();
        topPanel.add(donutChart, "grow");

        mainContent.add(topPanel, "growx, wrap");

        // --- Bottom: Recycling Log ---
        JPanel logPanel = new JPanel(new BorderLayout());
        logPanel.setBorder(BorderFactory.createTitledBorder("Garbage Collector Logs (Permanently removed messages)"));

        JButton clearBtn = new JButton("Clear GC Logs", new DeleteIcon(16));
        clearBtn.addActionListener(e -> {
            agi.getContextManager().getGarbageCollector().clearLog();
            refreshLogTable();
        });
        JPanel topLogActions = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 2));
        topLogActions.setOpaque(false);
        topLogActions.add(clearBtn);
        logPanel.add(topLogActions, BorderLayout.NORTH);

        logModel = new DefaultTableModel(new Object[]{"Timestamp", "Msg ID", "Type", "Tokens Recycled"}, 0) {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };
        logTable = new JXTable(logModel);
        
        JPanel tableContainer = new JPanel(new BorderLayout());
        tableContainer.add(logTable.getTableHeader(), BorderLayout.NORTH);
        tableContainer.add(logTable, BorderLayout.CENTER);
        logPanel.add(tableContainer, BorderLayout.CENTER);

        mainContent.add(logPanel, "grow");

        add(mainContent, BorderLayout.CENTER);
        

    }

    /**
     * Helper factory method for creating standardized value labels.
     * These labels are styled with a specific font and color to maintain 
     * visual consistency across different token categories (System, Tools, History, etc.).
     * 
     * @param font The font to apply to the label.
     * @param color The foreground color for the label text.
     * @return A newly constructed and styled JLabel.
     */
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
        if (isShowing()) {
            refresh();
        }
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
        
        new SwingTask<ContextWindowGarbageCollector.Stats>(agiPanel, "CwGC Metabolism Check", () -> {
            gc.calculate();
            return gc.getStats();
        }, stats -> {
            int threshold = cm.getTokenThreshold();
            
            boolean inject = agi.getRequestConfig().isInjectInbandMetadata();
            String strategyName = inject ? "In-Band Injection" : "Consolidated Index";
            strategyLabel.setText("<html><font color='#666666'>Strategy:</font> <font color='#333333'><b>" + strategyName + "</b></font></html>");

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
        }).start();
    }

    /**
     * Synchronizes the recycling log table with the latest records from the 
     * {@link ContextWindowGarbageCollector}. This method performs a partial refresh
     * by comparing record counts to optimize EDT performance.
     */
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
     * {@inheritDoc} 
     * <p>Triggers an initial metabolism refresh when the panel is added to a container, 
     * ensuring that the visualization is up-to-date as soon as it becomes visible to the user.</p> 
     */
    @Override
    public void addNotify() {
        super.addNotify();
    }
}
