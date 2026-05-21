/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.config;

import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;
import java.util.stream.Collectors;
import javax.swing.BoxLayout;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.SpinnerNumberModel;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.RequestConfig;
import uno.anahata.asi.agi.provider.ServerTool;
import uno.anahata.asi.agi.provider.ThinkingLevel;
import uno.anahata.asi.swing.components.ScrollablePanel;
import uno.anahata.asi.swing.components.SliderSpinner;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;

/**
 * A panel for editing the model-specific execution configuration (RequestConfig).
 * Manages parameters like temperature, thinking levels, modalities, and server tools.
 * Supports "Model Default" checkboxes to allow sending nulls to the API.
 * 
 * @author anahata
 */
@Slf4j
public class RequestConfigPanel extends ScrollablePanel implements PropertyChangeListener {

    /**
     * The underlying request execution configuration being edited.
     */
    private final RequestConfig config;
    /**
     * The parent AGI session context used for model metadata discovery.
     */
    private final Agi agi;

    //== Parameter Components ==//
    /**
     * Interactive slider for configuring temperature settings.
     */
    private SliderSpinner temperatureControl;
    /**
     * Checkbox to toggle model-default temperature.
     */
    private JCheckBox temperatureDefaultCheckbox;
    
    /**
     * Interactive slider for configuring maximum output tokens.
     */
    private SliderSpinner maxOutputTokensControl;
    /**
     * Checkbox to toggle model-default output tokens limits.
     */
    private JCheckBox maxOutputTokensDefaultCheckbox;
    
    /**
     * Interactive slider for configuring Top-K parameter.
     */
    private SliderSpinner topKControl;
    /**
     * Checkbox to toggle model-default Top-K settings.
     */
    private JCheckBox topKDefaultCheckbox;
    
    /**
     * Interactive slider for configuring Top-P settings.
     */
    private SliderSpinner topPControl;
    /**
     * Checkbox to toggle model-default Top-P parameter.
     */
    private JCheckBox topPDefaultCheckbox;
    
    /**
     * Interactive slider to configure generated candidate response counts.
     */
    private SliderSpinner candidateCountControl;
    /**
     * Dropdown selection component for thinking level configuration.
     */
    private JComboBox<ThinkingLevel> thinkingLevelDropdown;
    /**
     * Checkbox to toggle provider-native tool call schemas.
     */
    private JCheckBox useNativeSchemasCheckbox;

    //== Modalities & Tools ==//
    /**
     * Panel hosting dynamic response modality checkboxes.
     */
    private JPanel modalitiesPanel;
    /**
     * Panel hosting dynamic server tools toggle checkboxes.
     */
    private JPanel serverToolsPanel;

    //== Debug/Context Flags ==//
    /**
     * Checkbox to toggle including soft-pruned items in payload.
     */
    private JCheckBox includePrunedCheckbox;
    /**
     * Checkbox to toggle in-band prompt metadata injection.
     */
    private JCheckBox injectInbandMetadataCheckbox;

    /**
     * Constructs a new panel for the specified RequestConfig.
     * @param config The request configuration to edit.
     * @param agi The associated Agi session (for model discovery).
     */
    public RequestConfigPanel(RequestConfig config, Agi agi) {
        this.config = config;
        this.agi = agi;
        initComponents();
        loadConfig();
        
        if (agi != null) {
            agi.addPropertyChangeListener(this);
            new EdtPropertyChangeListener(this, agi.getConfig(), "hostedToolsEnabled", evt -> loadConfig());
        }
    }

    /**
     * Initializes and arranges Swing layouts and section panels.
     */
    private void initComponents() {
        setLayout(new MigLayout("fillx, insets 10", "[grow,fill]", "[]10[]10[]"));

        // --- 1. MODEL PARAMETERS ---
        JPanel paramsPanel = createSectionPanel("Model Parameters");
        paramsPanel.setLayout(new MigLayout("insets 10, hidemode 3", "[]10[]10[grow,fill,pref:pref:500]"));

        paramsPanel.add(new JLabel("Thinking Level:"));
        thinkingLevelDropdown = new JComboBox<>(ThinkingLevel.values());
        thinkingLevelDropdown.setRenderer(new DefaultListCellRenderer() {
            @Override
            public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
                super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                if (value instanceof ThinkingLevel tl) setText(tl.getDisplayValue());
                return this;
            }
        });
        paramsPanel.add(thinkingLevelDropdown, "span 2, wrap");

        paramsPanel.add(new JLabel("Temperature:"));
        temperatureDefaultCheckbox = new JCheckBox("Use Model's Default");
        paramsPanel.add(temperatureDefaultCheckbox);
        temperatureControl = new SliderSpinner(new SpinnerNumberModel(1.0, 0.0, 2.0, 0.1), 0, 200, 100.0);
        paramsPanel.add(temperatureControl, "wrap");

        paramsPanel.add(new JLabel("Max Output Tokens:"));
        maxOutputTokensDefaultCheckbox = new JCheckBox("Use Model's Default");
        paramsPanel.add(maxOutputTokensDefaultCheckbox);
        maxOutputTokensControl = new SliderSpinner(new SpinnerNumberModel(65000, 1, 1000000, 1), 1, 1000000, 1.0);
        paramsPanel.add(maxOutputTokensControl, "wrap");

        paramsPanel.add(new JLabel("Top K:"));
        topKDefaultCheckbox = new JCheckBox("Use Model's Default");
        paramsPanel.add(topKDefaultCheckbox);
        topKControl = new SliderSpinner(new SpinnerNumberModel(64, 1, 100, 1), 1, 100, 1.0);
        paramsPanel.add(topKControl, "wrap");

        paramsPanel.add(new JLabel("Top P:"));
        topPDefaultCheckbox = new JCheckBox("Use Model's Default");
        paramsPanel.add(topPDefaultCheckbox);
        topPControl = new SliderSpinner(new SpinnerNumberModel(0.95, 0.0, 1.0, 0.05), 0, 100, 100.0);
        paramsPanel.add(topPControl, "wrap");

        paramsPanel.add(new JLabel("Max Candidates:"));
        candidateCountControl = new SliderSpinner(new SpinnerNumberModel(1, 1, 8, 1), 1, 8, 1.0);
        paramsPanel.add(candidateCountControl, "skip 1, wrap");

        add(paramsPanel, "wrap");

        // --- 2. FEATURES & TOOLS ---
        JPanel featuresPanel = createSectionPanel("Features & Capabilities");
        featuresPanel.setLayout(new MigLayout("fillx, insets 10", "[right]pref[10][grow,fill]"));

        useNativeSchemasCheckbox = new JCheckBox("Use provider-native tool schemas");
        featuresPanel.add(new JLabel("Native Schemas:"));
        featuresPanel.add(useNativeSchemasCheckbox, "wrap");

        modalitiesPanel = new JPanel();
        modalitiesPanel.setLayout(new BoxLayout(modalitiesPanel, BoxLayout.Y_AXIS));
        modalitiesPanel.setOpaque(false);
        featuresPanel.add(new JLabel("Modalities:"), "top");
        featuresPanel.add(modalitiesPanel, "wrap");

        serverToolsPanel = new JPanel();
        serverToolsPanel.setLayout(new BoxLayout(serverToolsPanel, BoxLayout.Y_AXIS));
        serverToolsPanel.setOpaque(false);
        featuresPanel.add(new JLabel("Server Tools:"), "top");
        featuresPanel.add(serverToolsPanel, "wrap");

        add(featuresPanel, "wrap");

        // --- 3. CONTEXT & DEBUG ---
        JPanel debugPanel = createSectionPanel("Context & Debug Flags");
        debugPanel.setLayout(new MigLayout("fillx, insets 10", "[right]pref[10][grow,fill]"));

        includePrunedCheckbox = new JCheckBox("Include soft-pruned items in API payload");
        debugPanel.add(new JLabel("Include Pruned:"));
        debugPanel.add(includePrunedCheckbox, "wrap");

        injectInbandMetadataCheckbox = new JCheckBox("Inject IDs and depths into model prompt");
        debugPanel.add(new JLabel("In-band Metadata:"));
        debugPanel.add(injectInbandMetadataCheckbox, "wrap");

        add(debugPanel, "wrap");

        setupListeners();
    }

    /**
     * Binds action and change events to update the underlying RequestConfig.
     */
    private void setupListeners() {
        thinkingLevelDropdown.addActionListener(e -> config.setThinkingLevel((ThinkingLevel) thinkingLevelDropdown.getSelectedItem()));
        
        temperatureDefaultCheckbox.addActionListener(e -> {
            boolean isDefault = temperatureDefaultCheckbox.isSelected();
            temperatureControl.setEnabled(!isDefault);
            if (isDefault) {
                config.setTemperature(null);
                refreshEffectiveDefaults();
            } else {
                config.setTemperature(((Number) temperatureControl.getValue()).floatValue());
            }
        });
        temperatureControl.addChangeListener(e -> {
            if (!temperatureDefaultCheckbox.isSelected()) {
                config.setTemperature(((Number) temperatureControl.getValue()).floatValue());
            }
        });

        maxOutputTokensDefaultCheckbox.addActionListener(e -> {
            boolean isDefault = maxOutputTokensDefaultCheckbox.isSelected();
            maxOutputTokensControl.setEnabled(!isDefault);
            if (isDefault) {
                config.setMaxOutputTokens(null);
                refreshEffectiveDefaults();
            } else {
                config.setMaxOutputTokens((Integer) maxOutputTokensControl.getValue());
            }
        });
        maxOutputTokensControl.addChangeListener(e -> {
            if (!maxOutputTokensDefaultCheckbox.isSelected()) {
                config.setMaxOutputTokens((Integer) maxOutputTokensControl.getValue());
            }
        });

        topKDefaultCheckbox.addActionListener(e -> {
            boolean isDefault = topKDefaultCheckbox.isSelected();
            topKControl.setEnabled(!isDefault);
            if (isDefault) {
                config.setTopK(null);
                refreshEffectiveDefaults();
            } else {
                config.setTopK((Integer) topKControl.getValue());
            }
        });
        topKControl.addChangeListener(e -> {
            if (!topKDefaultCheckbox.isSelected()) {
                config.setTopK((Integer) topKControl.getValue());
            }
        });

        topPDefaultCheckbox.addActionListener(e -> {
            boolean isDefault = topPDefaultCheckbox.isSelected();
            topPControl.setEnabled(!isDefault);
            if (isDefault) {
                config.setTopP(null);
                refreshEffectiveDefaults();
            } else {
                config.setTopP(((Number) topPControl.getValue()).floatValue());
            }
        });
        topPControl.addChangeListener(e -> {
            if (!topPDefaultCheckbox.isSelected()) {
                config.setTopP(((Number) topPControl.getValue()).floatValue());
            }
        });

        candidateCountControl.addChangeListener(e -> config.setCandidateCount((Integer) candidateCountControl.getValue()));
        useNativeSchemasCheckbox.addActionListener(e -> config.setUseNativeSchemas(useNativeSchemasCheckbox.isSelected()));
        
        includePrunedCheckbox.addActionListener(e -> config.setIncludePruned(includePrunedCheckbox.isSelected()));
        injectInbandMetadataCheckbox.addActionListener(e -> config.setInjectInbandMetadata(injectInbandMetadataCheckbox.isSelected()));
    }

    /**
     * Loads configuration states from RequestConfig to UI components.
     */
    private void loadConfig() {
        thinkingLevelDropdown.setSelectedItem(config.getThinkingLevel());
        useNativeSchemasCheckbox.setSelected(config.isUseNativeSchemas());
        includePrunedCheckbox.setSelected(config.isIncludePruned());
        injectInbandMetadataCheckbox.setSelected(config.isInjectInbandMetadata());

        temperatureDefaultCheckbox.setSelected(config.getTemperature() == null);
        temperatureControl.setEnabled(config.getTemperature() != null);
        
        maxOutputTokensDefaultCheckbox.setSelected(config.getMaxOutputTokens() == null);
        maxOutputTokensControl.setEnabled(config.getMaxOutputTokens() != null);
        
        topKDefaultCheckbox.setSelected(config.getTopK() == null);
        topKControl.setEnabled(config.getTopK() != null);
        
        topPDefaultCheckbox.setSelected(config.getTopP() == null);
        topPControl.setEnabled(config.getTopP() != null);

        candidateCountControl.setValue(config.getCandidateCount() != null ? config.getCandidateCount() : 1);

        refreshEffectiveDefaults();
    }

    /**
     * Updates the UI components with either the explicitly set config value
     * or the model's default value if the config value is null.
     */
    private void refreshEffectiveDefaults() {
        AbstractModel model = agi != null ? agi.getSelectedModel() : null;

        // Temperature
        float temp = config.getTemperature() != null ? config.getTemperature() : 
                     (model != null && model.getDefaultTemperature() != null ? model.getDefaultTemperature() : 1.0f);
        temperatureControl.setValue((double) temp);

        // Max Tokens
        int modelMax = (model != null && model.getMaxOutputTokens() > 0) ? model.getMaxOutputTokens() : 8192;
        int maxTokens = config.getMaxOutputTokens() != null ? config.getMaxOutputTokens() : modelMax;
        
        maxOutputTokensControl.getSlider().setMaximum(Math.max(modelMax, 4096));
        maxOutputTokensControl.setValue(maxTokens);

        // Top K
        int topK = config.getTopK() != null ? config.getTopK() : 
                   (model != null && model.getDefaultTopK() != null ? model.getDefaultTopK() : 40);
        topKControl.setValue(topK);

        // Top P
        float topP = config.getTopP() != null ? config.getTopP() : 
                     (model != null && model.getDefaultTopP() != null ? model.getDefaultTopP() : 0.95f);
        topPControl.setValue((double) topP);

        if (model != null) {
            updateModalities(model);
            updateServerTools(model);
        }
    }

    /**
     * Dynamically populates modalities toggles supported by the selected model.
     * @param model The active AI model.
     */
    private void updateModalities(AbstractModel model) {
        modalitiesPanel.removeAll();
        for (String modality : model.getSupportedResponseModalities()) {
            JCheckBox cb = new JCheckBox(modality);
            cb.setSelected(config.getResponseModalities().contains(modality));
            cb.setOpaque(false);
            cb.addActionListener(e -> {
                if (cb.isSelected()) config.getResponseModalities().add(modality);
                else config.getResponseModalities().remove(modality);
            });
            modalitiesPanel.add(cb);
        }
    }

    /**
     * Dynamically populates server tool checkboxes based on available model tools.
     * @param model The active AI model.
     */
    private void updateServerTools(AbstractModel model) {
        serverToolsPanel.removeAll();
        List<Object> enabledIds = config.getEnabledServerTools().stream().map(ServerTool::getId).collect(Collectors.toList());
        for (ServerTool tool : model.getAvailableServerTools()) {
            JCheckBox cb = new JCheckBox(tool.getDisplayName());
            cb.setToolTipText(tool.getDescription());
            cb.setSelected(enabledIds.contains(tool.getId()));
            cb.setOpaque(false);
            cb.addActionListener(e -> {
                if (cb.isSelected()) config.getEnabledServerTools().add(tool);
                else config.getEnabledServerTools().removeIf(st -> st.getId().equals(tool.getId()));
            });
            serverToolsPanel.add(cb);
        }
    }

    /**
     * Helper to create a unified titled border panel with Barça design colors.
     * @param title The section title.
     * @return a beautifully styled JPanel container.
     */
    private JPanel createSectionPanel(String title) {
        JPanel panel = new JPanel();
        panel.setOpaque(false);
        panel.setBorder(javax.swing.BorderFactory.createTitledBorder(
                javax.swing.BorderFactory.createLineBorder(new java.awt.Color(200, 200, 200)),
                title, 0, 0, getFont().deriveFont(java.awt.Font.BOLD, 12f), new java.awt.Color(100, 100, 100)));
        return panel;
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        if ("selectedModel".equals(evt.getPropertyName())) {
            // When model changes, we only need to refresh the effective defaults shown in the UI
            // and the enabled tools/modalities. We don't call loadConfig() because that 
            // would reset the "Default" checkboxes if the user had already tweaked them.
            refreshEffectiveDefaults();
        }
    }
}
