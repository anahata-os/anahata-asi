/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import javax.swing.filechooser.FileNameExtensionFilter;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.AgiConfig;
import java.util.List;
import java.util.stream.Collectors;
import javax.swing.*;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXTextField;
import org.jdesktop.swingx.autocomplete.AutoCompleteDecorator;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.persistence.kryo.KryoUtils;
import uno.anahata.asi.agi.provider.AbstractAgiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.swing.icons.SaveSessionIcon;
import uno.anahata.asi.swing.icons.SearchIcon;
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.swing.provider.AgiProviderRegistryViewer;

/**
 * The header panel for the agi UI, containing the agi nickname, session controls,
 * and provider/model selection components.
 *
 * @author anahata
 */
@Slf4j
public class HeaderPanel extends JPanel {
    private static final int ICON_SIZE = 24;

    /** The parent aggregator panel providing access to session and config. */
    private final AgiPanel agiPanel;
    /** The active agi session orchestrator. */
    private Agi agi;

    /** The text field for the session's nickname, synchronized with the domain on focus loss. */
    private JXTextField nicknameField;
    /** The button to trigger a manual session save and export to a file. */
    private JButton saveSessionButton;
    /** The selector for the AI provider, populates the model selector on change. */
    private JComboBox<AbstractAgiProvider> providerComboBox;
    /** The selector for the specific AI model, supports autocompletion via {@link AutoCompleteDecorator}. */
    private JComboBox<AbstractModel> modelComboBox;
    /** The button to open the global model registry viewer for deep exploration. */
    private JButton searchModelsButton;

    /** 
     * Constructs the header panel and initializes references.
     * 
     * @param agiPanel The parent aggregator panel.
     */
    public HeaderPanel(AgiPanel agiPanel) {
        this.agiPanel = agiPanel;
        this.agi = agiPanel.getAgi();
        log.info("Header Panel constructor, selected agi model: " + agi.getSelectedModel());        
    }

    /** 
     * Initializes the UI components using MigLayout and populates the model selectors.
     */
    public void initComponents() {
        setLayout(new MigLayout("insets 5, fillx, gap 10",
                                "[][]push[][][]", // Nickname, Save, PUSH, Provider, Model, Search
                                "[]")); // Row constraints

        // Nickname Field
        nicknameField = new JXTextField("Nickname");
        nicknameField.setText(agi.getNickname());
        nicknameField.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent e) {
                agi.setNickname(nicknameField.getText());
            }
        });
        add(nicknameField, "w 150!");

        // Session Buttons
        saveSessionButton = new JButton(new SaveSessionIcon(ICON_SIZE));
        saveSessionButton.setToolTipText("Save Session");
        saveSessionButton.addActionListener(e -> saveSession());
        add(saveSessionButton);

        // Provider ComboBox (Right-aligned, skipping the push column)
        providerComboBox = new JComboBox<>();
        providerComboBox.setToolTipText("Select AI Provider");
        providerComboBox.setRenderer(new ProviderRenderer());
        add(providerComboBox, "skip 1, w 150!");

        // Model ComboBox
        modelComboBox = new JComboBox<>();
        modelComboBox.setToolTipText("Select Model (autocomplete enabled)");
        modelComboBox.setRenderer(new ModelRenderer());
        AutoCompleteDecorator.decorate(modelComboBox);
        add(modelComboBox, "w 200!");

        // Search Button
        searchModelsButton = new JButton(new SearchIcon(ICON_SIZE));
        searchModelsButton.setToolTipText("Search and view all available models");
        add(searchModelsButton);
        
        // Populate providers and models first
        populateProviders();

        // Select the agi's currently selected model if available
        AbstractModel selectedAgiModel = agi.getSelectedModel();
        System.out.println("Preselecting " + selectedAgiModel);
        if (selectedAgiModel != null) {
            // First, find and set the provider
            for (int i = 0; i < providerComboBox.getItemCount(); i++) {
                AbstractAgiProvider provider = providerComboBox.getItemAt(i);
                if (provider.getProviderId().equals(selectedAgiModel.getProviderId())) {
                    log.info("Preselecting provider " + provider);
                    providerComboBox.setSelectedItem(provider);
                    // Explicitly update models for the selected provider after setting the provider
                    updateModelsForSelectedProvider(); 
                    break;
                }
            }
            
            // Then, find and set the model
            for (int i = 0; i < modelComboBox.getItemCount(); i++) {
                if (modelComboBox.getItemAt(i).getModelId().equals(selectedAgiModel.getModelId())) {
                    log.info("Preselecting model " + modelComboBox.getItemAt(i));
                    modelComboBox.setSelectedItem(modelComboBox.getItemAt(i));
                    break;
                }
            }
        } else if (providerComboBox.getItemCount() > 0) { // Original logic if no model is pre-selected
            providerComboBox.setSelectedIndex(0);
            updateModelsForSelectedProvider();
            if (modelComboBox.getItemCount() > 0) {
                modelComboBox.setSelectedIndex(0);
                agi.setSelectedModel((AbstractModel) modelComboBox.getSelectedItem());
            }
        }
        
        // Add listeners AFTER initial population and selection
        addListeners();
    }

    /**
     * Reloads the panel with the new agi state.
     */
    public void reload() {
        this.agi = agiPanel.getAgi();
        removeAll();
        initComponents();
        revalidate();
        repaint();
    }

    /** 
     * Fetches all registered providers from the agi session and adds them to the combo box.
     */
    private void populateProviders() {
        List<AbstractAgiProvider> providers = agi.getProviders();
        for (AbstractAgiProvider provider : providers) {
            providerComboBox.addItem(provider);
        }
    }

    /** 
     * Installs action listeners for provider and model selection.
     */
    private void addListeners() {
        providerComboBox.addActionListener(e -> updateModelsForSelectedProvider());
        
        modelComboBox.addActionListener(e -> {
            AbstractModel selectedModel = (AbstractModel) modelComboBox.getSelectedItem();
            if (selectedModel != null) {
                agi.setSelectedModel(selectedModel);
            }
        });
        
        searchModelsButton.addActionListener(e -> showProviderRegistry());
    }
    
    /** 
     * Opens the provider registry viewer dialog to search and select models from all providers.
     */
    private void showProviderRegistry() {
        // Collect all models from all providers
        List<AbstractModel> allModels = agi.getProviders().stream()
            .flatMap(provider -> provider.getModels().stream())
            .collect(Collectors.toList());

        JDialog dialog = new JDialog(SwingUtilities.getWindowAncestor(this), "AI Provider & Model Registry", JDialog.ModalityType.MODELESS);
        
        AgiProviderRegistryViewer viewer = new AgiProviderRegistryViewer(allModels, selectedModel -> {
            // Handle model selection: set the model in the combo box and close the dialog
            modelComboBox.setSelectedItem(selectedModel);
            dialog.dispose();
        });
        
        dialog.getContentPane().add(viewer);
        dialog.setPreferredSize(new Dimension(1200, 800));
        dialog.pack();
        dialog.setLocationRelativeTo(this);
        dialog.setVisible(true);
    }

    /** 
     * Updates the model combo box items based on the currently selected provider.
     */
    private void updateModelsForSelectedProvider() {
        AbstractAgiProvider selectedProvider = (AbstractAgiProvider) providerComboBox.getSelectedItem();
        modelComboBox.removeAllItems();
        if (selectedProvider != null) {
            List<? extends AbstractModel> models = selectedProvider.getModels();
            for (AbstractModel model : models) {
                modelComboBox.addItem(model);
            }
        }
    }

    /** 
     * Triggers a manual save and exports the session to a .kryo file chosen by the user.
     */
    private void saveSession() {
        new SwingTask<>(this, "Save Session", () -> {
            // 1. Perform standard auto-save
            agi.save();

            // 2. Open File Chooser for manual "Save As"
            SwingUtilities.invokeLater(() -> {
                AgiConfig config = agi.getConfig();
                AbstractAsiContainer container = config.getAsiContainer();
                Path savedDir = container.getSavedSessionsDir();
                
                String nickname = agi.getNickname();
                String defaultName = (nickname != null && !nickname.isBlank()) ? nickname : config.getSessionId();
                if (!defaultName.endsWith(".kryo")) {
                    defaultName += ".kryo";
                }

                JFileChooser chooser = new JFileChooser(savedDir.toFile());
                chooser.setDialogTitle("Save Session As...");
                chooser.setSelectedFile(new File(savedDir.toFile(), defaultName));
                chooser.setFileFilter(new FileNameExtensionFilter("Anahata Sessions (*.kryo)", "kryo"));

                if (chooser.showSaveDialog(this) == JFileChooser.APPROVE_OPTION) {
                    File targetFile = chooser.getSelectedFile();
                    if (!targetFile.getName().endsWith(".kryo")) {
                        targetFile = new File(targetFile.getParentFile(), targetFile.getName() + ".kryo");
                    }
                    
                    final File finalFile = targetFile;
                    new SwingTask<>(this, "Exporting Session", () -> {
                        try {
                            byte[] data = KryoUtils.serialize(agi);
                            Files.write(finalFile.toPath(), data);
                            log.info("Session exported successfully to: {}", finalFile.getAbsolutePath());
                        } catch (Exception ex) {
                            log.error("Failed to export session", ex);
                            throw ex;
                        }
                        return null;
                    }).execute();
                }
            });
            return null;
        }).execute();
    }

    /** Custom renderer to display provider's display name. */
    private static class ProviderRenderer extends DefaultListCellRenderer {
        /** 
         * {@inheritDoc} 
         * <p>Renders the provider's ID in the list.</p> 
         */
        @Override
        public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
            super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
            if (value instanceof AbstractAgiProvider) {
                setText(((AbstractAgiProvider) value).getProviderId());
            }
            return this;
        }
    }

    /** Custom renderer to display model's display name. */
    private static class ModelRenderer extends DefaultListCellRenderer {
        /** 
         * {@inheritDoc} 
         * <p>Renders the model's display name in the list, ensuring it follows the 
         * agi's selected model format.</p> 
         */
        @Override
        public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
            super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
            if (value instanceof AbstractModel) {
                setText(((AbstractModel) value).getDisplayName());
            }
            return this;
        }
    }
}
