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
import javax.swing.Icon;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JScrollPane;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.AsiContainerPreferences;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.swing.agi.config.AgiConfigPanel;
import uno.anahata.asi.swing.agi.config.RequestConfigPanel;
import uno.anahata.asi.swing.icons.AddIcon;
import uno.anahata.asi.swing.icons.CancelIcon;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.icons.RestartIcon;
import uno.anahata.asi.swing.icons.SaveIcon;
import uno.anahata.asi.swing.icons.SettingsIcon;
import uno.anahata.asi.swing.internal.SwingTask;

import uno.anahata.asi.swing.components.ScrollablePanel;

/**
 * A centralized, multi-tabbed Command Center for managing the ASI container.
 * It governs global defaults, DNA templates, and per-provider API key pools.
 * 
 * @author anahata
 */
@Slf4j
public class AsiContainerPreferencesPanel extends ScrollablePanel {
 
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
 
    /** The master tabbed pane for configuration categories. */
    private final JTabbedPane mainTabs;
    
    /** Callback to close the host dialog or frame. */
    private Runnable closeCallback;

    /** The primary action button to persist changes. */
    private JButton saveBtn;
    /** The action button to discard changes and exit. */
    private JButton cancelBtn;
    /** The specialized button to restore factory DNA. */
    private JButton resetBtn;
    
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
        mainTabs.addTab("Agi Config", createScrollPane(createAgiTemplateTab()));
        mainTabs.addTab("Request Config", createScrollPane(createRequestTemplateTab()));
        mainTabs.addTab("Tool Permissions", createScrollPane(new ToolkitPermissionsPanel(container)));
        mainTabs.addTab("AI Providers", createAiProvidersTab());

        if (initialTabIndex >= 0 && initialTabIndex < mainTabs.getTabCount()) {
            mainTabs.setSelectedIndex(initialTabIndex);
        }

        add(mainTabs, BorderLayout.CENTER);
        add(createBottomButtonPanel(), BorderLayout.SOUTH);

        // Initialize model discovery
        refreshModelDropdown();
    }

    /**
     * Standardizes the creation of JScrollPanes for use within preferences tabs, 
     * ensuring consistent scroll speed and borderless rendering.
     * 
     * @param c The component to wrap.
     * @return A configured JScrollPane.
     */
    private JScrollPane createScrollPane(Component c) {
        JScrollPane scroll = new JScrollPane(c);
        scroll.setBorder(null);
        scroll.getVerticalScrollBar().setUnitIncrement(24);
        return scroll;
    }

    /**
     * Constructs the bottom command bar with the 'Reset' action anchored to 
     * the left and 'Save/Cancel' to the right.
     * 
     * @return The populated action panel.
     */
    private JPanel createBottomButtonPanel() {
        JPanel panel = new JPanel(new BorderLayout());
        panel.setOpaque(false);
        panel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        resetBtn = new JButton("Reset to Defaults", new RestartIcon(16));
        resetBtn.setToolTipText("Wipe current templates and restore from Anahata factory defaults.");
        resetBtn.addActionListener(e -> handleReset());

        JPanel rightPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 10, 0));
        rightPanel.setOpaque(false);
        
        saveBtn = new JButton("Save", new SaveIcon(16));
        saveBtn.addActionListener(e -> handleSave());

        cancelBtn = new JButton("Cancel", new CancelIcon(16));
        cancelBtn.addActionListener(e -> {
            if (closeCallback != null) closeCallback.run();
        });

        rightPanel.add(saveBtn);
        rightPanel.add(cancelBtn);

        panel.add(resetBtn, BorderLayout.WEST);
        panel.add(rightPanel, BorderLayout.EAST);

        return panel;
    }

    /**
     * Triggers the synchronized save sequence and invokes the close callback 
     * upon success.
     */
    private void handleSave() {
        try {
            save();
            if (closeCallback != null) closeCallback.run();
            JOptionPane.showMessageDialog(this, "Global configuration saved and applied.", "Success", JOptionPane.INFORMATION_MESSAGE);
        } catch (Exception ex) {
            log.error("Failed to save preferences", ex);
            JOptionPane.showMessageDialog(this, "Failed to save: " + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
        }
    }

    /**
     * Wipes the template configurations and hot-reloads the UI components 
     * to reflect the new DNA.
     */
    private void handleReset() {
        int choice = JOptionPane.showConfirmDialog(this,
                "Are you sure you want to reset the Agi and Request configurations to factory defaults?\n" +
                "Your API keys and Providers will NOT be affected.",
                "Reset to Defaults", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

        if (choice == JOptionPane.YES_OPTION) {
            prefs.resetAgiTemplate(container);
            prefs.setRequestTemplate(new uno.anahata.asi.agi.provider.RequestConfig(null));
            
            // UI Hot-Reload: Re-initialize the tabs to reflect new template
            int selected = mainTabs.getSelectedIndex();
            mainTabs.removeAll();
            mainTabs.addTab("General Defaults", createGeneralTab());
            mainTabs.addTab("Agi Config", createScrollPane(createAgiTemplateTab()));
            mainTabs.addTab("Request Config", createScrollPane(createRequestTemplateTab()));
            mainTabs.addTab("Tool Permissions", createScrollPane(new ToolkitPermissionsPanel(container)));
            mainTabs.addTab("AI Providers", createAiProvidersTab());
            mainTabs.setSelectedIndex(selected);
            
            JOptionPane.showMessageDialog(this, "Configurations reset successfully. Please click Save to persist.");
        }
    }

    /**
     * Sets the callback to close the host window.
     * @param closeCallback the callback.
     */
    public void setCloseCallback(Runnable closeCallback) {
        this.closeCallback = closeCallback;
    }


    /**
     * Programmatically selects a specific tab in the preferences dashboard.
     * 
     * @param index The index of the tab to select.
     */
    public void selectTab(int index) {
        if (index >= 0 && index < mainTabs.getTabCount()) {
            mainTabs.setSelectedIndex(index);
        }
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
    private JPanel createAgiTemplateTab() {
        JPanel panel = new JPanel(new BorderLayout());
        
        AgiConfigPanel templatesPanel = new AgiConfigPanel(prefs.getAgiTemplate());
        panel.add(templatesPanel, BorderLayout.CENTER);
        
        JLabel header = new JLabel("<html><div style='padding: 10px;'><b>Agi Config:</b> These settings are inherited by every new session born in this container. Changes made here do not affect existing sessions.</div></html>");
        panel.add(header, BorderLayout.NORTH);
        
        return panel;
    }

    private JPanel createRequestTemplateTab() {
        JPanel panel = new JPanel(new BorderLayout());
        RequestConfigPanel reqPanel = new RequestConfigPanel(
                prefs.getRequestTemplate(), 
                null
        );
        panel.add(reqPanel, BorderLayout.CENTER);
        JLabel header = new JLabel("<html><div style='padding: 10px;'><b>Request Config:</b> These settings are inherited by every new session born in this container.</div></html>");
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
        JButton addBtn = new JButton("Add New Provider", new AddIcon(16));
        addBtn.addActionListener(e -> {
            showAddProviderDialog(providerTabs);
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
            Icon icon = IconUtils.getIcon("aiproviders/" + p.getClass().getName() + ".png", 16, 16);
            providerTabs.addTab(p.getDisplayName(), icon, createScrollPane(keysPanel));
            activeProviderPanels.add(keysPanel);
        }
        
        // 2. Draft/Unsaved Providers
        for (AbstractAiProvider p : unsavedProviders) {
            AiProviderPanel keysPanel = new AiProviderPanel(containerPanel, p, () -> {
                unsavedProviders.remove(p);
                refreshProviderTabs(providerTabs);
            });
            Icon icon = IconUtils.getIcon("aiproviders/" + p.getClass().getName() + ".png", 16, 16);
            providerTabs.addTab("<html><b>* " + p.getDisplayName() + "</b></html>", icon, createScrollPane(keysPanel));
            activeProviderPanels.add(keysPanel);
        }
    }

    private void showAddProviderDialog(JTabbedPane providerTabs) {
        List<Class<? extends AbstractAiProvider>> classes = AbstractSwingAsiContainer.AVAILABLE_PROVIDER_CLASSES;
        JComboBox<ProviderItem> combo = new JComboBox<>();
        for (Class<? extends AbstractAiProvider> clazz : classes) {
            try {
                AbstractAiProvider tempInstance = clazz.getDeclaredConstructor().newInstance();
                combo.addItem(new ProviderItem(clazz, tempInstance.getDisplayName()));
            } catch (Exception ex) {
                combo.addItem(new ProviderItem(clazz, clazz.getSimpleName()));
            }
        }
        
        int result = JOptionPane.showConfirmDialog(this, combo, "Select Provider Type", JOptionPane.OK_CANCEL_OPTION);
        if (result == JOptionPane.OK_OPTION) {
            ProviderItem selected = (ProviderItem) combo.getSelectedItem();
            if (selected != null) {
                try {
                    AbstractAiProvider newProvider = selected.clazz().getDeclaredConstructor().newInstance();
                    newProvider.setUuid(java.util.UUID.randomUUID().toString());
                    unsavedProviders.add(newProvider);
                    refreshProviderTabs(providerTabs);
                    providerTabs.setSelectedIndex(providerTabs.getTabCount() - 1);
                } catch (Exception ex) {
                    log.error("Failed to instantiate provider", ex);
                    JOptionPane.showMessageDialog(this, "Failed to create provider: " + ex.getMessage());
                }
            }
        }
    }

    private record ProviderItem(Class<? extends AbstractAiProvider> clazz, String displayName) {
        @Override
        public String toString() {
            return displayName;
        }
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
