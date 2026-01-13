/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.swing.chat;

import com.jgoodies.forms.layout.FormLayout;
import java.awt.BorderLayout;
import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;
import java.util.stream.Collectors;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SpinnerNumberModel;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.core.RequestConfig;
import uno.anahata.ai.model.provider.AbstractModel;
import uno.anahata.ai.model.provider.ServerTool;
import uno.anahata.ai.swing.components.SliderSpinner;

/**
 * A panel for editing the RequestConfig of a Chat session.
 * It uses JGoodies FormLayout for a professional, left-aligned form.
 * 
 * @author anahata
 */
@Slf4j
public class RequestConfigPanel extends JPanel implements PropertyChangeListener {

    /** The parent chat panel. */
    private final ChatPanel chatPanel;
    /** The chat session. */
    private final Chat chat;
    /** The request configuration being edited. */
    private final RequestConfig config;

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
    /** Checkbox for session-level streaming toggle. */
    private JCheckBox streamingCheckbox;
    /** Panel for response modalities checkboxes. */
    private JPanel modalitiesPanel;
    /** Panel for server tools checkboxes. */
    private JPanel serverToolsPanel;

    /**
     * Constructs a new RequestConfigPanel.
     * 
     * @param chatPanel The parent chat panel.
     */
    public RequestConfigPanel(ChatPanel chatPanel) {
        this.chatPanel = chatPanel;
        this.chat = chatPanel.getChat();
        this.config = chat.getConfig().getRequestConfig();
        initComponents();
        loadConfig();
        
        chat.addPropertyChangeListener(this);
    }

    /**
     * Initializes the UI components and layout using JGoodies FormLayout.
     */
    private void initComponents() {
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        // FormLayout: 
        // Column 1: Label (right aligned)
        // Column 2: Gap
        // Column 3: Control (fill, grows)
        FormLayout layout = new FormLayout(
            "right:pref, 4dlu, fill:pref:grow",
            "pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 10dlu, pref, 10dlu, pref"
        );
        
        JPanel mainPanel = new JPanel(layout);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        int row = 1;

        // Stream Tokens (Session Level)
        mainPanel.add(new JLabel("Stream Tokens:"), "1, " + row);
        streamingCheckbox = new JCheckBox();
        mainPanel.add(streamingCheckbox, "3, " + row);
        row += 2;

        // Temperature
        mainPanel.add(new JLabel("Temperature:"), "1, " + row);
        temperatureControl = new SliderSpinner(new SpinnerNumberModel(1.0, 0.0, 2.0, 0.1), 0, 200, 100.0);
        mainPanel.add(temperatureControl, "3, " + row);
        row += 2;

        // Max Output Tokens
        mainPanel.add(new JLabel("Max Output Tokens:"), "1, " + row);
        maxOutputTokensControl = new SliderSpinner(new SpinnerNumberModel(2048, 1, 1000000, 1), 1, 1000000, 1.0);
        mainPanel.add(maxOutputTokensControl, "3, " + row);
        row += 2;

        // Top K
        mainPanel.add(new JLabel("Top K:"), "1, " + row);
        topKControl = new SliderSpinner(new SpinnerNumberModel(40, 1, 100, 1), 1, 100, 1.0);
        mainPanel.add(topKControl, "3, " + row);
        row += 2;

        // Top P
        mainPanel.add(new JLabel("Top P:"), "1, " + row);
        topPControl = new SliderSpinner(new SpinnerNumberModel(0.95, 0.0, 1.0, 0.05), 0, 100, 100.0);
        mainPanel.add(topPControl, "3, " + row);
        row += 2;
        
        // Candidate Count
        mainPanel.add(new JLabel("Max Candidates:"), "1, " + row);
        candidateCountControl = new SliderSpinner(new SpinnerNumberModel(1, 1, 8, 1), 1, 8, 1.0);
        mainPanel.add(candidateCountControl, "3, " + row);
        row += 2;

        // Response Modalities
        mainPanel.add(new JLabel("Response Modalities:"), "1, " + row);
        modalitiesPanel = new JPanel();
        modalitiesPanel.setLayout(new BoxLayout(modalitiesPanel, BoxLayout.Y_AXIS));
        mainPanel.add(modalitiesPanel, "3, " + row);
        row += 2;

        // Server Tools
        mainPanel.add(new JLabel("Server Tools:"), "1, " + row);
        serverToolsPanel = new JPanel();
        serverToolsPanel.setLayout(new BoxLayout(serverToolsPanel, BoxLayout.Y_AXIS));
        mainPanel.add(serverToolsPanel, "3, " + row);

        add(new JScrollPane(mainPanel), BorderLayout.CENTER);

        // Add listeners to update config
        temperatureControl.addChangeListener(e -> {
            config.setTemperature(((Number) temperatureControl.getValue()).floatValue());
        });

        maxOutputTokensControl.addChangeListener(e -> {
            config.setMaxOutputTokens((Integer) maxOutputTokensControl.getValue());
        });

        topKControl.addChangeListener(e -> {
            config.setTopK((Integer) topKControl.getValue());
        });

        topPControl.addChangeListener(e -> {
            config.setTopP(((Number) topPControl.getValue()).floatValue());
        });
        
        candidateCountControl.addChangeListener(e -> {
            config.setCandidateCount((Integer) candidateCountControl.getValue());
        });
        
        streamingCheckbox.addActionListener(e -> {
            chat.getConfig().setStreaming(streamingCheckbox.isSelected());
        });
    }

    /**
     * Loads the current configuration into the UI components.
     * This method prioritizes user-set values in the RequestConfig,
     * falling back to model defaults if necessary.
     */
    private void loadConfig() {
        AbstractModel model = chat.getSelectedModel();
        
        streamingCheckbox.setSelected(chat.getConfig().isStreaming());
        
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

        if (model != null) {
            updateModalities(model);
            updateServerTools(model);
        }
    }

    /**
     * Updates the response modalities checkboxes based on the selected model.
     * 
     * @param model The selected model.
     */
    private void updateModalities(AbstractModel model) {
        modalitiesPanel.removeAll();
        for (String modality : model.getSupportedResponseModalities()) {
            JCheckBox cb = new JCheckBox(modality);
            cb.setAlignmentX(Component.LEFT_ALIGNMENT);
            cb.setSelected(config.getResponseModalities().contains(modality));
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
     * Updates the server tools checkboxes based on the selected model.
     * 
     * @param model The selected model.
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
     * Handles property change events from the parent Chat session.
     * Specifically, it listens for "selectedModel" changes to refresh the UI.
     * 
     * @param evt The property change event.
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        if ("selectedModel".equals(evt.getPropertyName())) {
            loadConfig();
        }
    }
}
