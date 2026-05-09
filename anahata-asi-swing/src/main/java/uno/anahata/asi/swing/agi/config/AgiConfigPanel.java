/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.config;

import java.awt.Color;
import java.awt.Font;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.TitledBorder;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.ExpandToolsPreference;
import uno.anahata.asi.swing.components.ScrollablePanel;

/**
 * A panel for editing the framework-level configuration of an Agi session.
 * Manages loop logic, retry policies, and metabolic depths (DNA).
 * 
 * @author anahata
 */
@Slf4j
public class AgiConfigPanel extends ScrollablePanel implements PropertyChangeListener {

    private final AgiConfig config;

    //== Loop Settings ==//
    private JCheckBox streamingCheckbox;
    private JCheckBox includeThoughtsCheckbox;
    private JCheckBox expandThoughtsCheckbox;
    private JComboBox<ExpandToolsPreference> expandToolsDropdown;
    private JCheckBox autoReplyToolsCheckbox;

    //== Retry Settings ==//
    private JSpinner apiMaxRetriesSpinner;
    private JSpinner apiInitialDelaySpinner;
    private JSpinner apiMaxDelaySpinner;

    //== Metabolic Settings ==//
    private JSpinner tokenThresholdSpinner;
    private JSpinner textMaxDepthSpinner;
    private JSpinner toolMaxDepthSpinner;
    private JSpinner blobMaxDepthSpinner;
    private JSpinner thoughtMaxDepthSpinner;

    /**
     * Constructs a new panel for the specified AgiConfig.
     * @param config The configuration to edit.
     */
    public AgiConfigPanel(AgiConfig config) {
        this.config = config;
        initComponents();
        loadConfig();
        config.addPropertyChangeListener(this);
    }

    private void initComponents() {
        setLayout(new MigLayout("fillx, insets 10", "[grow,fill]", "[]10[]10[]"));
        // --- 1. LOOP PANEL ---
        JPanel loopPanel = createSectionPanel("Loop Logic & Behavior");
        loopPanel.setLayout(new MigLayout("fillx, insets 10", "[right]pref[10][grow,fill]"));

        streamingCheckbox = new JCheckBox("Stream model responses in real-time");
        loopPanel.add(new JLabel("Stream Tokens:"));
        loopPanel.add(streamingCheckbox, "wrap");
        includeThoughtsCheckbox = new JCheckBox("Request internal reasoning (COT)");
        loopPanel.add(new JLabel("Include Thoughts:"));
        loopPanel.add(includeThoughtsCheckbox, "wrap");
        expandThoughtsCheckbox = new JCheckBox("Expand thought blocks in Chat");
        loopPanel.add(new JLabel("Expand Thoughts:"));
        loopPanel.add(expandThoughtsCheckbox, "wrap");
        expandToolsDropdown = new JComboBox<>(ExpandToolsPreference.values());
        loopPanel.add(new JLabel("Expand Tools:"));
        loopPanel.add(expandToolsDropdown, "wrap");
        autoReplyToolsCheckbox = new JCheckBox("Automatically re-prompt after tool execution");
        loopPanel.add(new JLabel("Auto-Reply Tools:"));
        loopPanel.add(autoReplyToolsCheckbox, "wrap");
        add(loopPanel, "wrap");

        // --- 2. RETRY PANEL ---
        JPanel retryPanel = createSectionPanel("API Retry Policy");
        retryPanel.setLayout(new MigLayout("fillx, insets 10", "[right]pref[10][grow,fill]"));
        apiMaxRetriesSpinner = new JSpinner(new SpinnerNumberModel(5, 0, 20, 1));
        retryPanel.add(new JLabel("Max Retries:"));
        retryPanel.add(apiMaxRetriesSpinner, "wrap");
        apiInitialDelaySpinner = new JSpinner(new SpinnerNumberModel(2000L, 0L, 10000L, 100L));
        retryPanel.add(new JLabel("Initial Delay (ms):"));
        retryPanel.add(apiInitialDelaySpinner, "wrap");
        apiMaxDelaySpinner = new JSpinner(new SpinnerNumberModel(30000L, 1000L, 300000L, 1000L));
        retryPanel.add(new JLabel("Max Delay (ms):"));
        retryPanel.add(apiMaxDelaySpinner, "wrap");
        add(retryPanel, "wrap");

        // --- 3. METABOLIC PANEL ---
        JPanel metabolismPanel = createSectionPanel("Context Metabolism (The DNA)");
        metabolismPanel.setLayout(new MigLayout("fillx, insets 10", "[right]pref[10][grow,fill]"));
        tokenThresholdSpinner = new JSpinner(new SpinnerNumberModel(250000, 1000, 2000000, 1000));
        metabolismPanel.add(new JLabel("Token Threshold:"));
        metabolismPanel.add(tokenThresholdSpinner, "wrap");
        textMaxDepthSpinner = new JSpinner(new SpinnerNumberModel(108, 1, 1000, 1));
        metabolismPanel.add(new JLabel("Text Max Depth:"));
        metabolismPanel.add(textMaxDepthSpinner, "wrap");
        toolMaxDepthSpinner = new JSpinner(new SpinnerNumberModel(12, 1, 1000, 1));
        metabolismPanel.add(new JLabel("Tool Max Depth:"));
        metabolismPanel.add(toolMaxDepthSpinner, "wrap");
        blobMaxDepthSpinner = new JSpinner(new SpinnerNumberModel(4, 1, 1000, 1));
        metabolismPanel.add(new JLabel("Blob Max Depth:"));
        metabolismPanel.add(blobMaxDepthSpinner, "wrap");
        thoughtMaxDepthSpinner = new JSpinner(new SpinnerNumberModel(12, 1, 1000, 1));
        metabolismPanel.add(new JLabel("Thought Max Depth:"));
        metabolismPanel.add(thoughtMaxDepthSpinner, "wrap");
        add(metabolismPanel, "wrap");
        setupListeners();
    }

    private void setupListeners() {
        streamingCheckbox.addActionListener(e -> config.setStreaming(streamingCheckbox.isSelected()));
        includeThoughtsCheckbox.addActionListener(e -> {
            boolean selected = includeThoughtsCheckbox.isSelected();
            config.setIncludeThoughts(selected);
            expandThoughtsCheckbox.setEnabled(selected);
        });
        expandThoughtsCheckbox.addActionListener(e -> config.setExpandThoughts(expandThoughtsCheckbox.isSelected()));
        expandToolsDropdown.addActionListener(e -> config.setExpandTools((ExpandToolsPreference) expandToolsDropdown.getSelectedItem()));
        autoReplyToolsCheckbox.addActionListener(e -> config.setAutoReplyTools(autoReplyToolsCheckbox.isSelected()));
        apiMaxRetriesSpinner.addChangeListener(e -> config.setApiMaxRetries((Integer) apiMaxRetriesSpinner.getValue()));
        apiInitialDelaySpinner.addChangeListener(e -> config.setApiInitialDelayMillis(((Number) apiInitialDelaySpinner.getValue()).longValue()));
        apiMaxDelaySpinner.addChangeListener(e -> config.setApiMaxDelayMillis(((Number) apiMaxDelaySpinner.getValue()).longValue()));
        tokenThresholdSpinner.addChangeListener(e -> config.setTokenThreshold((Integer) tokenThresholdSpinner.getValue()));
        textMaxDepthSpinner.addChangeListener(e -> config.setDefaultTextPartMaxDepth((Integer) textMaxDepthSpinner.getValue()));
        toolMaxDepthSpinner.addChangeListener(e -> config.setDefaultToolMaxDepth((Integer) toolMaxDepthSpinner.getValue()));
        blobMaxDepthSpinner.addChangeListener(e -> config.setDefaultBlobPartMaxDepth((Integer) blobMaxDepthSpinner.getValue()));
        thoughtMaxDepthSpinner.addChangeListener(e -> config.setDefaultThoughtPartMaxDepth((Integer) thoughtMaxDepthSpinner.getValue()));
    }

    private void loadConfig() {
        streamingCheckbox.setSelected(config.isStreaming());
        includeThoughtsCheckbox.setSelected(config.isIncludeThoughts());
        expandThoughtsCheckbox.setSelected(config.isExpandThoughts());
        expandThoughtsCheckbox.setEnabled(config.isIncludeThoughts());
        expandToolsDropdown.setSelectedItem(config.getExpandTools());
        autoReplyToolsCheckbox.setSelected(config.isAutoReplyTools());
        apiMaxRetriesSpinner.setValue(config.getApiMaxRetries());
        apiInitialDelaySpinner.setValue(config.getApiInitialDelayMillis());
        apiMaxDelaySpinner.setValue(config.getApiMaxDelayMillis());
        tokenThresholdSpinner.setValue(config.getTokenThreshold());
        textMaxDepthSpinner.setValue(config.getDefaultTextPartMaxDepth());
        toolMaxDepthSpinner.setValue(config.getDefaultToolMaxDepth());
        blobMaxDepthSpinner.setValue(config.getDefaultBlobPartMaxDepth());
        thoughtMaxDepthSpinner.setValue(config.getDefaultThoughtPartMaxDepth());
    }

    private JPanel createSectionPanel(String title) {
        JPanel panel = new JPanel();
        panel.setOpaque(false);
        TitledBorder border = BorderFactory.createTitledBorder(
                BorderFactory.createLineBorder(new Color(200, 200, 200)),
                title, TitledBorder.LEFT, TitledBorder.TOP,
                getFont().deriveFont(Font.BOLD, 12f), new Color(100, 100, 100)
        );
        panel.setBorder(border);
        return panel;
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        loadConfig();
    }
}
