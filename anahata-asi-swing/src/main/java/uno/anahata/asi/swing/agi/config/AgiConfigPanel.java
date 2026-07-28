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
 * <p>
 * This panel provides a comprehensive interface for managing the 'DNA' of an
 * AGI session, including loop logic, retry policies, and context window
 * metabolism. It uses {@link MigLayout} for responsive, grid-based arrangement.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class AgiConfigPanel extends ScrollablePanel implements PropertyChangeListener {

    /**
     * The underlying configuration model being edited.
     */
    private final AgiConfig config;

    //== Loop Settings ==//
    /**
     * Checkbox to toggle real-time token streaming.
     */
    private JCheckBox streamingCheckbox;
    /**
     * Checkbox to request model reasoning (Chain of Thought).
     */
    private JCheckBox includeThoughtsCheckbox;
    /**
     * Checkbox to control initial expansion of thought blocks in the chat UI.
     */
    private JCheckBox expandThoughtsCheckbox;
    /**
     * Dropdown for configuring tool call expansion preferences.
     */
    private JComboBox<ExpandToolsPreference> expandToolsDropdown;
    /**
     * Checkbox for automatic re-prompting after tool execution.
     */
    private JCheckBox autoReplyToolsCheckbox;

    //== Retry Settings ==//
    /**
     * Spinner for configuring the maximum number of API request retries.
     */
    private JSpinner apiMaxRetriesSpinner;
    /**
     * Spinner for configuring the initial backoff delay for retries.
     */
    private JSpinner apiInitialDelaySpinner;
    /**
     * Spinner for configuring the maximum backoff delay for retries.
     */
    private JSpinner apiMaxDelaySpinner;

    //== Metabolic Settings ==//
    /**
     * Spinner for setting the context window token threshold.
     */
    private JSpinner tokenThresholdSpinner;
    /**
     * Spinner for setting the text part maximum depth policy.
     */
    private JSpinner textMaxDepthSpinner;
    /**
     * Spinner for setting the tool call/response maximum depth policy.
     */
    private JSpinner toolMaxDepthSpinner;
    /**
     * Spinner for setting the blob (image/media) part maximum depth policy.
     */
    private JSpinner blobMaxDepthSpinner;
    /**
     * Spinner for setting the model thought part maximum depth policy.
     */
    private JSpinner thoughtMaxDepthSpinner;
    /**
     * Spinner for setting the hosted web search result maximum depth policy.
     */
    private JSpinner webSearchMaxDepthSpinner;
    /**
     * Spinner for setting the hosted code execution result maximum depth
     * policy.
     */
    private JSpinner codeExecutionMaxDepthSpinner;

    /**
     * Constructs a new panel for the specified AgiConfig.
     *
     * @param config The configuration to edit.
     */
    public AgiConfigPanel(AgiConfig config) {
        this.config = config;
        initComponents();
        loadConfig();
        config.addPropertyChangeListener(this);
    }

    /**
     * Initializes all UI components and defines the two-column grid layout.
     */
    private void initComponents() {
        setLayout(new MigLayout("fillx, insets 10 10 100 10", "[grow,fill]"));

        // Top Row to hold Loop and GC side-by-side
        JPanel topRow = new JPanel(new MigLayout("fillx, insets 0", "[grow,fill]10[grow,fill]"));
        topRow.setOpaque(false);

        // --- 1. LOOP PANEL ---
        JPanel loopPanel = createSectionPanel("Loop Logic & Behavior");
        loopPanel.setLayout(new MigLayout("fillx, insets 10", "[right]pref[10][grow,fill]"));

        tokenThresholdSpinner = new JSpinner(new SpinnerNumberModel(1000000, 1000, 2000000, 1000));
        loopPanel.add(new JLabel("Token Threshold:"));
        loopPanel.add(tokenThresholdSpinner, "split 2, w 100!");
        loopPanel.add(new JLabel("tokens"), "wrap, gapbottom 10");

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

        topRow.add(loopPanel, "growy");

        // --- 2. GC PANEL ---
        JPanel gcPanel = createSectionPanel("Context Window Garbage Collector");
        gcPanel.setLayout(new MigLayout("fillx, insets 10", "[right]pref[80!]40[right]pref[80!][grow]"));

        JLabel infoLabel = new JLabel("<html><b>Max Depth Defaults</b> (the number of turns after which the part will become effectively pruned).<br/>Individual Tools or Toolkits may override the default \"tools\" value.<br/>See Tools in Context Panel for more details</html>");
        infoLabel.setFont(infoLabel.getFont().deriveFont(11f));
        infoLabel.setForeground(new Color(120, 120, 120));
        gcPanel.add(infoLabel, "span 5, wrap, gapbottom 10");

        thoughtMaxDepthSpinner = new JSpinner(new SpinnerNumberModel(6, 1, 1000, 1));
        gcPanel.add(new JLabel("Thought:"));
        gcPanel.add(thoughtMaxDepthSpinner, "w 80!");
        webSearchMaxDepthSpinner = new JSpinner(new SpinnerNumberModel(8, 1, 1000, 1));
        gcPanel.add(new JLabel("Hosted Web Search:"));
        gcPanel.add(webSearchMaxDepthSpinner, "w 80!, wrap");

        textMaxDepthSpinner = new JSpinner(new SpinnerNumberModel(108, 1, 1000, 1));
        gcPanel.add(new JLabel("Text:"));
        gcPanel.add(textMaxDepthSpinner, "w 80!");
        codeExecutionMaxDepthSpinner = new JSpinner(new SpinnerNumberModel(8, 1, 1000, 1));
        gcPanel.add(new JLabel("Hosted Code Execution:"));
        gcPanel.add(codeExecutionMaxDepthSpinner, "w 80!, wrap");

        toolMaxDepthSpinner = new JSpinner(new SpinnerNumberModel(8, 1, 1000, 1));
        gcPanel.add(new JLabel("Tools:"));
        gcPanel.add(toolMaxDepthSpinner, "w 80!, wrap");

        blobMaxDepthSpinner = new JSpinner(new SpinnerNumberModel(4, 1, 1000, 1));
        gcPanel.add(new JLabel("Blobs:"));
        gcPanel.add(blobMaxDepthSpinner, "w 80!, wrap");

        topRow.add(gcPanel, "growy");

        add(topRow, "wrap, gapbottom 10");

        // --- 3. RETRY PANEL ---
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
        add(retryPanel, "wrap, gapbottom 20");

        setupListeners();
    }

    /**
     * Binds UI components to the underlying {@link AgiConfig} properties.
     */
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
        webSearchMaxDepthSpinner.addChangeListener(e -> config.setDefaultWebSearchMaxDepth((Integer) webSearchMaxDepthSpinner.getValue()));
        codeExecutionMaxDepthSpinner.addChangeListener(e -> config.setDefaultModelCodeExecutionMaxDepth((Integer) codeExecutionMaxDepthSpinner.getValue()));
    }

    /**
     * Loads current values from the {@link AgiConfig} into the UI components.
     */
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
        webSearchMaxDepthSpinner.setValue(config.getDefaultWebSearchMaxDepth());
        codeExecutionMaxDepthSpinner.setValue(config.getDefaultModelCodeExecutionMaxDepth());
    }

    /**
     * Helper for creating styled section panels with titled borders.
     *
     * @param title The title for the border.
     * @return The configured JPanel.
     */
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

    /**
     * {@inheritDoc}
     * <p>
     * Synchronizes the UI when external configuration changes occur.</p>
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        loadConfig();
    }
}
