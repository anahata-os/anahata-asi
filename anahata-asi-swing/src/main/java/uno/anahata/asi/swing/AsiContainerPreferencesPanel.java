/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.AsiContainerPreferences;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.swing.agi.config.SessionConfigPanel;
import uno.anahata.asi.swing.icons.AddIcon;
import uno.anahata.asi.swing.icons.RestartIcon;
import uno.anahata.asi.swing.internal.SwingTask;

/**
 * A centralized, multi-tabbed Command Center for managing the ASI container.
 * It governs global defaults, DNA templates, and per-provider API key pools.
 * 
 * @author anahata
 */
@Slf4j
public class AsiContainerPreferencesPanel extends JPanel {
 
    /** The parent container panel providing access to the global executor. */
    private final AbstractAsiContainerPanel containerPanel;
    /** The parent ASI container instance. */
    private final AbstractSwingAsiContainer container;
    /** The global ASI preferences being edited. */
    private final AsiContainerPreferences prefs;
    
    /** Dropdown for selecting the default AI provider for the container. */
    private JComboBox<String> providerDropdown;
    /** Dropdown for selecting the default AI model for the container. */
    private JComboBox<String> modelDropdown;
 
    private final JTabbedPane mainTabs;
    
    private final List<AbstractAiProvider> unsavedProviders = new ArrayList<>();
    private final List<AiProviderPanel> activeProviderPanels = new ArrayList<>();
 
    /**
     * Constructs a new preferences Command Center, defaulting to the first tab.
     * 
     * @param containerPanel The parent container dashboard panel.
     */
    public AsiContainerPreferencesPanel(AbstractAsiContainerPanel containerPanel) {
        this(containerPanel, 0);
    }
 
    /**
     * Constructs a new preferences Command Center with a specific tab selected.
     * 
     * @param containerPanel The parent container dashboard panel.
     * @param initialTabIndex The index of the tab to select initially.
     */
    public AsiContainerPreferencesPanel(AbstractAsiContainerPanel containerPanel, int initialTabIndex) {
        this.containerPanel = containerPanel;
        this.container = containerPanel.getAsiContainer();
        this.prefs = container.getPreferences();
        
        setLayout(new BorderLayout());
        setPreferredSize(new Dimension(950, 750));

        this.mainTabs = new JTabbedPane();
        
        if (prefs.isLoadFailed()) {
            JPanel alertPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
            alertPanel.setBackground(new java.awt.Color(0, 77, 152)); // Deep Barça Blue
            alertPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 4, 0, new java.awt.Color(165, 0, 68))); // Garnet accent line
            
            JLabel alertLabel = new JLabel("<html><div style='padding: 15px; text-align: center; color: white;'>" +
                "<span style='font-size: 14pt; color: #edbb00;'><b>Evolutionary Leap Detected!</b></span><br/>" +
                "<div style='margin-top: 5px;'>Your previous configuration has been gracefully archived to make room for new high-performance DNA.</div>" +
                "<i>Your previous settings were preserved as a 'Broken' backup in the preferences directory.</i></div></html>");
            alertPanel.add(alertLabel);
            add(alertPanel, BorderLayout.NORTH);
        }

        mainTabs.addTab("General Defaults", createGeneralTab());
        mainTabs.addTab("DNA Templates", createTemplatesTab());
        mainTabs.addTab("Tool Permissions", new ToolkitPermissionsPanel(container));
        mainTabs.addTab("AI Providers", createAiProvidersTab());

        if (initialTabIndex >= 0 && initialTabIndex < mainTabs.getTabCount()) {
            mainTabs.setSelectedIndex(initialTabIndex);
        }

        add(mainTabs, BorderLayout.CENTER);
        
        // Initialize model discovery
        refreshModelDropdown();
    }

    /**
     * Synchronizes all internal UI panels to their respective domain providers 
     * and persists the global container preferences to disk.
     * 
     * @throws IOException If the persistence layer fails.
     */
    public void save() throws IOException {
        // 1. Synchronize all open UI panels to their domain objects and key files
        for (AiProviderPanel panel : new ArrayList<>(activeProviderPanels)) {
            panel.syncToProvider();
        }

        // 2. Promote any "Draft" providers to the official container registry
        if (!unsavedProviders.isEmpty()) {
            for (AbstractAiProvider p : new ArrayList<>(unsavedProviders)) {
                container.registerProvider(p);
            }
            unsavedProviders.clear();
        }

        // 3. Persist everything to preferences.kryo
        container.savePreferences();

        // 4. Refresh UI state
        refreshProviderDropdown();

        log.info("Global preferences persisted to disk.");
    }

    /**
     * Creates and configures the 'General Defaults' tab.
     * 
     * @return The populated general settings panel.
     */
    private JPanel createGeneralTab() {
        JPanel panel = new JPanel(new MigLayout("fillx, insets 20", "[right]15[grow,fill]"));
        
        JLabel title = new JLabel("Global 'Starting XI' Settings");
        title.setFont(title.getFont().deriveFont(Font.BOLD, 16f));
        panel.add(title, "span, wrap, gapbottom 20");

        // 1. Default Provider
        panel.add(new JLabel("Default AI Provider:"));
        providerDropdown = new JComboBox<>();
        AgiConfig template = prefs.getAgiTemplate();
        DefaultComboBoxModel<String> providerModel = new DefaultComboBoxModel<>();
        template.getProviderUuids().forEach(providerModel::addElement);
        providerDropdown.setModel(providerModel);
        
        providerDropdown.setRenderer(new DefaultListCellRenderer() {
            @Override
            public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
                super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                if (value instanceof String uuid) {
                    AbstractAiProvider p = container.getProvider(uuid);
                    if (p != null) {
                        setText(p.getDisplayName() + " (" + p.getUuid() + ")");
                    } else {
                        setText(uuid);
                    }
                }
                return this;
            }
        });
        
        providerDropdown.setSelectedItem(template.getSelectedProviderUuid());
        providerDropdown.addActionListener(e -> {
            String selected = (String) providerDropdown.getSelectedItem();
            template.setSelectedProviderUuid(selected);
            refreshModelDropdown();
        });
        panel.add(providerDropdown, "wrap");

        // 2. Default Model
        panel.add(new JLabel("Default AI Model:"));
        modelDropdown = new JComboBox<>();
        modelDropdown.addActionListener(e -> {
            String selected = (String) modelDropdown.getSelectedItem();
            if (selected != null) {
                template.setSelectedModelId(selected);
            }
        });
        panel.add(modelDropdown, "wrap");
        
        panel.add(new JLabel("<html><font color='#707070'><i>These settings define which model is selected by default when you create a brand-new session.</i></font></html>"), "gapleft 20, span, wrap");

        return panel;
    }

    /**
     * Performs a background discovery of available models for the currently 
     * selected provider in the template.
     */
    private void refreshModelDropdown() {
        AgiConfig template = prefs.getAgiTemplate();
        String providerUuid = template.getSelectedProviderUuid();
        if (providerUuid == null) {
            return;
        }

        modelDropdown.setEnabled(false);
        modelDropdown.setToolTipText("Discovering models...");
 
        new SwingTask<List<String>>(containerPanel, "Model Discovery", () -> {
            AbstractAiProvider provider = container.getProvider(providerUuid);
            if (provider == null) {
                return new ArrayList<>();
            }
            return provider.getModels().stream()
                    .map(AbstractModel::getModelId)
                    .toList();
        }, models -> {
            DefaultComboBoxModel<String> model = new DefaultComboBoxModel<>();
            models.forEach(model::addElement);
            modelDropdown.setModel(model);
 
            if (template.getSelectedModelId() != null) {
                modelDropdown.setSelectedItem(template.getSelectedModelId());
            }
 
            // Fallback: If previous selection is invalid for the new provider, pick the first one.
            if (modelDropdown.getSelectedIndex() == -1 && model.getSize() > 0) {
                modelDropdown.setSelectedIndex(0);
            }
 
            modelDropdown.setEnabled(true);
            modelDropdown.setToolTipText(null);
        }, error -> {
            log.error("Failed to discover models for preferences", error);
            modelDropdown.setModel(new DefaultComboBoxModel<>(new String[]{"Discovery Failed"}));
            modelDropdown.setEnabled(true);
        }).start();
    }

    /**
     * Creates the 'DNA Templates' tab for managing global session blueprints.
     * 
     * @return The templates management panel.
     */
    private JPanel createTemplatesTab() {
        JPanel panel = new JPanel(new BorderLayout());
        
        boolean outdated = prefs.isAgiTemplateOutdated(container);
        
        // Toolbar for template actions
        JPanel toolbar = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        if (outdated) {
            JLabel warn = new JLabel("<html><font color='#e67e22'><b>DNA Evolution Detected!</b> Stored template toolset is out of sync with this version.</font></html>");
            toolbar.add(warn);
        }
        
        JButton resetBtn = new JButton("Reset DNA to Defaults", new RestartIcon(16));
        if (outdated) {
            resetBtn.setForeground(java.awt.Color.RED);
        }
        resetBtn.setToolTipText("Wipe current template and restore from Anahata factory defaults. Existing sessions are not affected.");
        resetBtn.addActionListener(e -> {
            int choice = JOptionPane.showConfirmDialog(this, 
                "Are you sure you want to reset the DNA template to factory defaults?\n" +
                "This will restore the default toolkits and policies for this version of Anahata.\n" +
                "Your API keys and Providers will NOT be affected.", 
                "Reset Template", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
            
            if (choice == JOptionPane.YES_OPTION) {
                prefs.resetAgiTemplate(container);
                mainTabs.setComponentAt(mainTabs.indexOfTab("DNA Templates"), createTemplatesTab());
                mainTabs.setSelectedIndex(mainTabs.indexOfTab("DNA Templates"));
                JOptionPane.showMessageDialog(this, "DNA Template reset successfully. Please click Save to persist.");
            }
        });
        toolbar.add(resetBtn);
        panel.add(toolbar, BorderLayout.SOUTH);

        // DNA Templates use the SessionConfigPanel aggregator bound to the global templates
        SessionConfigPanel templatesPanel = new SessionConfigPanel(
                prefs.getAgiTemplate(), 
                prefs.getRequestTemplate(), 
                null // No live Agi session
        );
        
        panel.add(templatesPanel, BorderLayout.CENTER);
        
        JLabel header = new JLabel("<html><div style='padding: 10px;'><b>Session DNA Templates:</b> These settings are inherited by every new session born in this container. Changes made here do not affect existing sessions.</div></html>");
        panel.add(header, BorderLayout.NORTH);
        
        return panel;
    }

    /**
     * Creates the 'AI Providers' tab for configuring endpoint-specific settings.
     * 
     * @return The provider configuration panel.
     */
    private JPanel createAiProvidersTab() {
        JPanel panel = new JPanel(new BorderLayout());
        JTabbedPane providerTabs = new JTabbedPane(JTabbedPane.LEFT);

        refreshProviderTabs(providerTabs);

        panel.add(providerTabs, BorderLayout.CENTER);

        // Toolbar for adding providers
        JPanel toolbar = new JPanel(new FlowLayout(FlowLayout.LEFT));
        JButton addBtn = new JButton("Add OpenAI Compatible Provider", new AddIcon(16));
        addBtn.addActionListener(e -> {
            addDraftOpenAiProvider(providerTabs);
        });
        toolbar.add(addBtn);

        panel.add(toolbar, BorderLayout.NORTH);

        return panel;
    }

    /**
     * Refreshes the provider sub-tabs to reflect newly added or removed providers.
     * 
     * @param providerTabs The tabbed pane containing individual provider panels.
     */
    private void refreshProviderTabs(JTabbedPane providerTabs) {
        providerTabs.removeAll();
        activeProviderPanels.clear();
        // 1. Existing Providers
        for (AbstractAiProvider p : container.getAllProviders()) {
            AiProviderPanel keysPanel = new AiProviderPanel(containerPanel, p, () -> {
                removeProvider(p, providerTabs);
            });
            providerTabs.addTab(p.getDisplayName(), keysPanel);
            activeProviderPanels.add(keysPanel);
        }
        
        // 2. Draft/Unsaved Providers
        for (AbstractAiProvider p : unsavedProviders) {
            AiProviderPanel keysPanel = new AiProviderPanel(containerPanel, p, () -> {
                unsavedProviders.remove(p);
                refreshProviderTabs(providerTabs);
            });
            providerTabs.addTab("<html><b>* " + p.getDisplayName() + "</b></html>", keysPanel);
            activeProviderPanels.add(keysPanel);
        }
    }

    /**
     * Adds a new, unsaved OpenAI-compatible provider to the temporary draft list.
     * 
     * @param providerTabs The tabbed pane to select the new provider in.
     */
    private void addDraftOpenAiProvider(JTabbedPane providerTabs) {
        String uuid = java.util.UUID.randomUUID().toString();
        uno.anahata.asi.openai.compatible.OpenAiCompatibleProvider draft = 
                new uno.anahata.asi.openai.compatible.OpenAiCompatibleProvider(uuid, "New Provider", "https://api.openai.com/v1", null, null);
        unsavedProviders.add(draft);
        refreshProviderTabs(providerTabs);
        providerTabs.setSelectedIndex(providerTabs.getTabCount() - 1);
    }

    /**
     * Removes a provider from both the container and the template.
     * 
     * @param provider The provider instance to remove.
     * @param providerTabs The UI container to refresh after removal.
     */
    private void removeProvider(AbstractAiProvider provider, JTabbedPane providerTabs) {
        String uuid = provider.getUuid();
        String name = provider.getDisplayName();
        
        int choice = JOptionPane.showConfirmDialog(this, 
                "Are you sure you want to remove the provider '" + name + "'?\n" +
                "This will unregister it from the container preferences.", 
                "Remove Provider", JOptionPane.YES_NO_OPTION);
        
        if (choice == javax.swing.JOptionPane.YES_OPTION) {
            prefs.getAgiTemplate().getProviderUuids().remove(uuid);
            container.unregisterProvider(uuid);
            // Note: We don't delete the provider's directory, just unregister it
            container.savePreferences();
            refreshProviderTabs(providerTabs);
            refreshProviderDropdown();
        }
    }

    /**
     * Refreshes the primary provider selection dropdown in the General tab.
     */
    private void refreshProviderDropdown() {
        AgiConfig template = prefs.getAgiTemplate();
        DefaultComboBoxModel<String> providerModel = new DefaultComboBoxModel<>();
        template.getProviderUuids().forEach(providerModel::addElement);
        providerDropdown.setModel(providerModel);
        providerDropdown.setSelectedItem(template.getSelectedProviderUuid());
    }

}
