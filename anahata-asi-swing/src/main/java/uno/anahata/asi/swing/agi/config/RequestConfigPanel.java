/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.config;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;
import java.util.stream.Collectors;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTabbedPane;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.TitledBorder;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.provider.RequestConfig;
import uno.anahata.asi.agi.provider.ThinkingLevel;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.ServerTool;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.components.ScrollablePanel;
import uno.anahata.asi.swing.components.SliderSpinner;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;

/**
 * A panel for editing the RequestConfig and AgiConfig of a Agi session.
 * It provides a structured view with tabs for better organization.
 * 
 * @author anahata
 */
@Slf4j
public class RequestConfigPanel extends ScrollablePanel implements PropertyChangeListener {

    /** The parent agi panel. */
    private final AgiPanel agiPanel;
    /** The agi session. */
    private final Agi agi;
    /** The request configuration being edited. */
    private final RequestConfig config;

    //== Request Configuration Components ==//
    /** Combined component for temperature. */
    private SliderSpinner temperatureControl;
    /** Combined component for max output tokens. */
    private SliderSpinner maxOutputTokensControl;
    /** Combined component for top K. */
    private SliderSpinner topKControl;
    /** Combined component for top P. */
    private SliderSpinner topPControl;
    /** Combined component for candidate count. */
    private SliderSpinner candidateCountControl;
    /** Dropdown for selecting the thinking level. */
    private JComboBox<ThinkingLevel> thinkingLevelDropdown;
    /** Checkbox for enabling native schemas. */
    private JCheckBox useNativeSchemasCheckbox;
    /** Panel for response modalities checkboxes. */
    private JPanel modalitiesPanel;
    /** Panel for server tools checkboxes. */
    private JPanel serverToolsPanel;

    //== Agi Loop Settings Components ==//
    /** Checkbox for session-level streaming toggle. */
    private JCheckBox streamingCheckbox;
    /** Checkbox for including thoughts in the response. */
    private JCheckBox includeThoughtsCheckbox;
    /** Checkbox for expanding thoughts by default. */
    private JCheckBox expandThoughtsCheckbox;
    /** Spinner for maximum API retries. */
    private JSpinner apiMaxRetriesSpinner;
    /** Spinner for initial API delay. */
    private JSpinner apiInitialDelaySpinner;
    /** Spinner for maximum API delay. */
    private JSpinner apiMaxDelaySpinner;

    //== Metabolic Settings Components ==//
    /** Spinner for token threshold. */
    private JSpinner tokenThresholdSpinner;
    /** Spinner for default text max depth. */
    private JSpinner textMaxDepthSpinner;
    /** Spinner for default tool max depth. */
    private JSpinner toolMaxDepthSpinner;
    /** Spinner for default blob max depth. */
    private JSpinner blobMaxDepthSpinner;
    /** Spinner for default thought max depth. */
    private JSpinner thoughtMaxDepthSpinner;    
    /** Checkbox for injecting in-band metadata. */
    private JCheckBox injectInbandMetadataCheckbox;
    /** Checkbox for including pruned parts in the API request. */
    private JCheckBox includePrunedCheckbox;

    /**
     * Constructs a new RequestConfigPanel.
     * 
     * @param agiPanel The parent agi panel.
     */
    public RequestConfigPanel(AgiPanel agiPanel) {
        this.agiPanel = agiPanel;
        this.agi = agiPanel.getAgi();
        this.config = agi.getRequestConfig();
        initComponents();
        loadConfig();
        
        agi.addPropertyChangeListener(this);
        // Listen to global config changes to refresh tool checkboxes
        new EdtPropertyChangeListener(this, agi.getConfig(), "hostedToolsEnabled", evt -> loadConfig());
    }

    /**
     * Orchestrates the construction of the entire UI tree for the configuration panel.
     * This method organizes settings into three primary logical tabs: Request, Loop, and Metabolic,
     * ensuring a clean separation between model-specific parameters and framework-level execution logic.
     */
    private void initComponents() {
        setLayout(new BorderLayout());

        JTabbedPane tabbedPane = new JTabbedPane();
        tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);

        // --- 1. REQUEST TAB ---
        JPanel requestTab = new JPanel(new MigLayout("fillx, insets 10", "[grow,fill]", "[]"));
        requestTab.setOpaque(false);
        
        JPanel requestPanel = createSectionPanel("Model Parameters");
        requestPanel.setLayout(new MigLayout("fillx, insets 10", "[right]pref[10][grow,fill]"));

        requestPanel.add(new JLabel("Thinking Level:"));
        thinkingLevelDropdown = new JComboBox<>(ThinkingLevel.values());
        thinkingLevelDropdown.setRenderer(new DefaultListCellRenderer() {
            /**
             * {@inheritDoc}
             * <p>Customizes the display of {@link ThinkingLevel} items by using their 
             * descriptive display values instead of the enum names.</p>
             */
            @Override
            public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
                super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                if (value instanceof ThinkingLevel tl) {
                    setText(tl.getDisplayValue());
                }
                return this;
            }
        });
        requestPanel.add(thinkingLevelDropdown, "wrap");

        requestPanel.add(new JLabel("Temperature:"));
        temperatureControl = new SliderSpinner(new SpinnerNumberModel(1.0, 0.0, 2.0, 0.1), 0, 200, 100.0);
        requestPanel.add(temperatureControl, "wrap");

        requestPanel.add(new JLabel("Max Output Tokens:"));
        maxOutputTokensControl = new SliderSpinner(new SpinnerNumberModel(2048, 1, 1000000, 1), 1, 1000000, 1.0);
        requestPanel.add(maxOutputTokensControl, "wrap");

        requestPanel.add(new JLabel("Top K:"));
        topKControl = new SliderSpinner(new SpinnerNumberModel(40, 1, 100, 1), 1, 100, 1.0);
        requestPanel.add(topKControl, "wrap");

        requestPanel.add(new JLabel("Top P:"));
        topPControl = new SliderSpinner(new SpinnerNumberModel(0.95, 0.0, 1.0, 0.05), 0, 100, 100.0);
        requestPanel.add(topPControl, "wrap");

        requestPanel.add(new JLabel("Max Candidates:"));
        candidateCountControl = new SliderSpinner(new SpinnerNumberModel(1, 1, 8, 1), 1, 8, 1.0);
        requestPanel.add(candidateCountControl, "wrap");

        requestPanel.add(new JLabel("Native Schemas:"));
        useNativeSchemasCheckbox = new JCheckBox("Use provider-native tool schemas");
        requestPanel.add(useNativeSchemasCheckbox, "wrap");

        requestPanel.add(new JLabel("Response Modalities:"), "top");
        modalitiesPanel = new JPanel();
        modalitiesPanel.setLayout(new BoxLayout(modalitiesPanel, BoxLayout.Y_AXIS));
        modalitiesPanel.setOpaque(false);
        requestPanel.add(modalitiesPanel, "wrap");

        requestPanel.add(new JLabel("Server Tools:"), "top");
        serverToolsPanel = new JPanel();
        serverToolsPanel.setLayout(new BoxLayout(serverToolsPanel, BoxLayout.Y_AXIS));
        serverToolsPanel.setOpaque(false);
        requestPanel.add(serverToolsPanel, "wrap");

        requestTab.add(requestPanel, "growx, wrap");
        tabbedPane.addTab("Request", new JScrollPane(requestTab));

        // --- 2. LOOP TAB ---
        JPanel loopTab = new JPanel(new MigLayout("fillx, insets 10", "[grow,fill]", "[]"));
        loopTab.setOpaque(false);

        JPanel loopPanel = createSectionPanel("Loop Logic & Retries");
        loopPanel.setLayout(new MigLayout("fillx, insets 10", "[right]pref[10][grow,fill]"));

        loopPanel.add(new JLabel("Stream Tokens:"));
        streamingCheckbox = new JCheckBox("Stream model responses in real-time");
        loopPanel.add(streamingCheckbox, "wrap");

        loopPanel.add(new JLabel("Include Thoughts:"));
        includeThoughtsCheckbox = new JCheckBox("Request internal reasoning (COT)");
        loopPanel.add(includeThoughtsCheckbox, "wrap");

        loopPanel.add(new JLabel("Expand Thoughts:"));
        expandThoughtsCheckbox = new JCheckBox("Expand thought blocks in Chat");
        loopPanel.add(expandThoughtsCheckbox, "wrap");

        loopPanel.add(new JLabel("API Max Retries:"));
        apiMaxRetriesSpinner = new JSpinner(new SpinnerNumberModel(5, 0, 20, 1));
        loopPanel.add(apiMaxRetriesSpinner, "wrap");

        loopPanel.add(new JLabel("Initial Delay (ms):"));
        apiInitialDelaySpinner = new JSpinner(new SpinnerNumberModel(2000L, 0L, 10000L, 100L));
        loopPanel.add(apiInitialDelaySpinner, "wrap");

        loopPanel.add(new JLabel("Max Delay (ms):"));
        apiMaxDelaySpinner = new JSpinner(new SpinnerNumberModel(30000L, 1000L, 300000L, 1000L));
        loopPanel.add(apiMaxDelaySpinner, "wrap");

        loopTab.add(loopPanel, "growx, wrap");
        tabbedPane.addTab("Loop", new JScrollPane(loopTab));

        // --- 3. METABOLIC TAB ---
        JPanel metabolicTab = new JPanel(new MigLayout("fillx, insets 10", "[grow,fill]", "[]"));
        metabolicTab.setOpaque(false);

        JPanel metabolismPanel = createSectionPanel("Context & Metabolic Depths");
        metabolismPanel.setLayout(new MigLayout("fillx, insets 10", "[right]pref[10][grow,fill]"));

        metabolismPanel.add(new JLabel("Token Threshold:"));
        tokenThresholdSpinner = new JSpinner(new SpinnerNumberModel(250000, 1000, 2000000, 1000));
        metabolismPanel.add(tokenThresholdSpinner, "wrap");

        metabolismPanel.add(new JLabel("Text Max Depth:"));
        textMaxDepthSpinner = new JSpinner(new SpinnerNumberModel(108, 1, 1000, 1));
        metabolismPanel.add(textMaxDepthSpinner, "wrap");

        metabolismPanel.add(new JLabel("Tool Max Depth:"));
        toolMaxDepthSpinner = new JSpinner(new SpinnerNumberModel(12, 1, 1000, 1));
        metabolismPanel.add(toolMaxDepthSpinner, "wrap");

        metabolismPanel.add(new JLabel("Blob Max Depth:"));
        blobMaxDepthSpinner = new JSpinner(new SpinnerNumberModel(4, 1, 1000, 1));
        metabolismPanel.add(blobMaxDepthSpinner, "wrap");

        metabolismPanel.add(new JLabel("Thought Max Depth:"));
        thoughtMaxDepthSpinner = new JSpinner(new SpinnerNumberModel(12, 1, 1000, 1));
        metabolismPanel.add(thoughtMaxDepthSpinner, "wrap");

        metabolismPanel.add(new JLabel("Debug Options:"));
        includePrunedCheckbox = new JCheckBox("Include pruned parts in API request");
        metabolismPanel.add(includePrunedCheckbox, "wrap");

        metabolismPanel.add(new JLabel("History Metadata:"));
        injectInbandMetadataCheckbox = new JCheckBox("Inject in-band metadata (IDs, Remaining Depth, Token Sizes, Pruned hints,etc.)");
        metabolismPanel.add(injectInbandMetadataCheckbox, "wrap");
        metabolicTab.add(metabolismPanel, "growx, wrap");
        tabbedPane.addTab("Metabolic", new JScrollPane(metabolicTab));

        add(tabbedPane, BorderLayout.CENTER);

        setupListeners();
    }

    /**
     * Creates a standardized, titled container panel for grouping related configuration settings.
     * This helper ensures visual consistency across all configuration sections by applying 
     * a uniform border and font styling.
     * 
     * @param title The localized title to be displayed on the section's border.
     * @return A newly constructed JPanel configured as a section container.
     */
    private JPanel createSectionPanel(String title) {
        JPanel panel = new JPanel();
        panel.setOpaque(false);
        TitledBorder border = BorderFactory.createTitledBorder(
                BorderFactory.createLineBorder(new Color(200, 200, 200)),
                title,
                TitledBorder.LEFT,
                TitledBorder.TOP,
                getFont().deriveFont(Font.BOLD, 12f),
                new Color(100, 100, 100)
        );
        panel.setBorder(border);
        return panel;
    }

    /**
     * Establishes reactive bindings between the UI components and the underlying domain models.
     * This method wires up listeners for both the provider-level {@link RequestConfig} and 
     * the framework-level {@link AgiConfig}, ensuring that user interactions are immediately 
     * reflected in the session state.
     */
    private void setupListeners() {
        // Request Config Listeners
        temperatureControl.addChangeListener(e -> config.setTemperature(((Number) temperatureControl.getValue()).floatValue()));
        maxOutputTokensControl.addChangeListener(e -> config.setMaxOutputTokens((Integer) maxOutputTokensControl.getValue()));
        topKControl.addChangeListener(e -> config.setTopK((Integer) topKControl.getValue()));
        topPControl.addChangeListener(e -> config.setTopP(((Number) topPControl.getValue()).floatValue()));
        candidateCountControl.addChangeListener(e -> config.setCandidateCount((Integer) candidateCountControl.getValue()));
        useNativeSchemasCheckbox.addActionListener(e -> config.setUseNativeSchemas(useNativeSchemasCheckbox.isSelected()));
        thinkingLevelDropdown.addActionListener(e -> config.setThinkingLevel((ThinkingLevel) thinkingLevelDropdown.getSelectedItem()));

        // Agi Config Listeners (Agi Loop)
        streamingCheckbox.addActionListener(e -> agi.getConfig().setStreaming(streamingCheckbox.isSelected()));
        includeThoughtsCheckbox.addActionListener(e -> {
            boolean selected = includeThoughtsCheckbox.isSelected();
            agi.getConfig().setIncludeThoughts(selected);
            expandThoughtsCheckbox.setEnabled(selected);
        });
        expandThoughtsCheckbox.addActionListener(e -> agi.getConfig().setExpandThoughts(expandThoughtsCheckbox.isSelected()));
        
        apiMaxRetriesSpinner.addChangeListener(e -> agi.getConfig().setApiMaxRetries((Integer) apiMaxRetriesSpinner.getValue()));
        apiInitialDelaySpinner.addChangeListener(e -> agi.getConfig().setApiInitialDelayMillis(((Number) apiInitialDelaySpinner.getValue()).longValue()));
        apiMaxDelaySpinner.addChangeListener(e -> agi.getConfig().setApiMaxDelayMillis(((Number) apiMaxDelaySpinner.getValue()).longValue()));

        // Metabolic Listeners
        tokenThresholdSpinner.addChangeListener(e -> agi.getConfig().setTokenThreshold((Integer) tokenThresholdSpinner.getValue()));
        textMaxDepthSpinner.addChangeListener(e -> agi.getConfig().setDefaultTextPartMaxDepth((Integer) textMaxDepthSpinner.getValue()));
        toolMaxDepthSpinner.addChangeListener(e -> agi.getConfig().setDefaultToolMaxDepth((Integer) toolMaxDepthSpinner.getValue()));
        blobMaxDepthSpinner.addChangeListener(e -> agi.getConfig().setDefaultBlobPartMaxDepth((Integer) blobMaxDepthSpinner.getValue()));
        thoughtMaxDepthSpinner.addChangeListener(e -> agi.getConfig().setDefaultThoughtPartMaxDepth((Integer) thoughtMaxDepthSpinner.getValue()));
        includePrunedCheckbox.addActionListener(e -> config.setIncludePruned(includePrunedCheckbox.isSelected()));
        injectInbandMetadataCheckbox.addActionListener(e -> config.setInjectInbandMetadata(injectInbandMetadataCheckbox.isSelected()));
    }

    /**
     * Synchronizes the UI components with the current state of the {@link Agi} session.
     * This method prioritizes explicit user-set values in the {@link RequestConfig}, 
     * falling back to the selected {@link AbstractModel} defaults when no override is present. 
     * It also refreshes model-dependent components like server tools and response modalities.
     */
    private void loadConfig() {
        AbstractModel model = agi.getSelectedModel();
        AgiConfig agiConfig = agi.getConfig();
        
        // Loop Settings
        streamingCheckbox.setSelected(agiConfig.isStreaming());
        includeThoughtsCheckbox.setSelected(agiConfig.isIncludeThoughts());
        expandThoughtsCheckbox.setSelected(agiConfig.isExpandThoughts());
        expandThoughtsCheckbox.setEnabled(agiConfig.isIncludeThoughts());
        
        apiMaxRetriesSpinner.setValue(agiConfig.getApiMaxRetries());
        apiInitialDelaySpinner.setValue(agiConfig.getApiInitialDelayMillis());
        apiMaxDelaySpinner.setValue(agiConfig.getApiMaxDelayMillis());

        // Request Settings
        thinkingLevelDropdown.setSelectedItem(config.getThinkingLevel());
        useNativeSchemasCheckbox.setSelected(config.isUseNativeSchemas());
        
        float temp = config.getTemperature() != null ? config.getTemperature() : (model != null && model.getDefaultTemperature() != null ? model.getDefaultTemperature() : 1.0f);
        temperatureControl.setValue((double) temp);
        
        int maxTokens = config.getMaxOutputTokens() != null ? config.getMaxOutputTokens() : (model != null ? model.getMaxOutputTokens() : 2048);
        if (model != null) {
            maxOutputTokensControl.getSlider().setMaximum(model.getMaxOutputTokens());
        }
        maxOutputTokensControl.setValue(maxTokens);
        
        int topK = config.getTopK() != null ? config.getTopK() : (model != null && model.getDefaultTopK() != null ? model.getDefaultTopK() : 40);
        topKControl.setValue(topK);
        
        float topP = config.getTopP() != null ? config.getTopP() : (model != null && model.getDefaultTopP() != null ? model.getDefaultTopP() : 0.95f);
        topPControl.setValue((double) topP);
        
        candidateCountControl.setValue(config.getCandidateCount() != null ? config.getCandidateCount() : 1);

        // Metabolic Settings
        tokenThresholdSpinner.setValue(agiConfig.getTokenThreshold());
        textMaxDepthSpinner.setValue(agiConfig.getDefaultTextPartMaxDepth());
        toolMaxDepthSpinner.setValue(agiConfig.getDefaultToolMaxDepth());
        blobMaxDepthSpinner.setValue(agiConfig.getDefaultBlobPartMaxDepth());
        thoughtMaxDepthSpinner.setValue(agiConfig.getDefaultThoughtPartMaxDepth());
        includePrunedCheckbox.setSelected(config.isIncludePruned());
        injectInbandMetadataCheckbox.setSelected(config.isInjectInbandMetadata());
        if (model != null) {
            updateModalities(model);
            updateServerTools(model);
        }
    }

    /**
     * Dynamically generates checkbox controls for the response modalities supported by the model.
     * This ensures the UI remains in sync with the current model's capabilities while 
     * allowing the user to toggle which modalities should be requested in the next API call.
     * 
     * @param model The active model whose modalities are being displayed.
     */
    private void updateModalities(AbstractModel model) {
        modalitiesPanel.removeAll();
        for (String modality : model.getSupportedResponseModalities()) {
            JCheckBox cb = new JCheckBox(modality);
            cb.setAlignmentX(Component.LEFT_ALIGNMENT);
            cb.setSelected(config.getResponseModalities().contains(modality));
            cb.setOpaque(false);
            cb.addActionListener(e -> {
                if (cb.isSelected()) {
                    config.getResponseModalities().add(modality);
                } else {
                    config.getResponseModalities().remove(modality);
                }
            });
            modalitiesPanel.add(cb);
        }
        modalitiesPanel.revalidate();
        modalitiesPanel.repaint();
    }

    /**
     * Dynamically generates checkbox controls for the server-side tools provided by the model.
     * This allows for granular control over hosted capabilities (like Google Search or 
     * Maps) that are available for the selected model.
     * 
     * @param model The active model whose server tools are being listed.
     */
    private void updateServerTools(AbstractModel model) {
        serverToolsPanel.removeAll();
        
        List<Object> enabledIds = config.getEnabledServerTools().stream()
                .map(ServerTool::getId)
                .collect(Collectors.toList());

        for (ServerTool tool : model.getAvailableServerTools()) {
            JCheckBox cb = new JCheckBox(tool.getDisplayName());
            cb.setAlignmentX(Component.LEFT_ALIGNMENT);
            cb.setToolTipText(tool.getDescription());
            cb.setSelected(enabledIds.contains(tool.getId()));
            cb.setOpaque(false);
            cb.addActionListener(e -> {
                if (cb.isSelected()) {
                    config.getEnabledServerTools().add(tool);
                } else {
                    config.getEnabledServerTools().removeIf(st -> st.getId().equals(tool.getId()));
                }
            });
            serverToolsPanel.add(cb);
        }
        serverToolsPanel.revalidate();
        serverToolsPanel.repaint();
    }

    /** 
     * {@inheritDoc} 
     * <p>Listens for structural changes in the {@link Agi} session, specifically targeting 
     * model swaps. When a new model is selected, this method triggers a full UI refresh 
     * to reflect the new model's parameters and capabilities.</p> 
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        if ("selectedModel".equals(evt.getPropertyName())) {
            loadConfig();
        }
    }
}
