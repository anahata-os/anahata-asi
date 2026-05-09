/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi;

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
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.swing.icons.SaveIcon;
import uno.anahata.asi.swing.icons.SearchIcon;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.swing.provider.AiProviderRegistryViewer;
import uno.anahata.asi.swing.provider.AiProviderRenderer;
import uno.anahata.asi.swing.provider.ModelRenderer;

/**
 * The header panel for the agi UI, containing the agi nickname, session
 * controls, and provider/model selection components.
 *
 * @author anahata
 */
@Slf4j
public class HeaderPanel extends JPanel {

    private static final int ICON_SIZE = 24;

    /**
     * The parent aggregator panel providing access to session and config.
     */
    private final AgiPanel agiPanel;
    /**
     * The active agi session orchestrator.
     */
    private Agi agi;

    /**
     * The text field for the session's nickname, synchronized with the domain
     * on focus loss.
     */
    private JXTextField nicknameField;
    /**
     * The button to trigger a manual session save and export to a file.
     * The button to trigger a manual session save and export to a file.
     */
    private JButton saveSessionButton;
    /**
     * The button to clone the current session.
     */
    private JButton cloneSessionButton;
    /**
     * The button to permanently dispose of the current session.
     */
    private JButton disposeSessionButton;
    /**
     * The selector for the AI provider, populates the model selector on change.
     */
    private JComboBox<AbstractAiProvider> providerComboBox;
    /**
     * The selector for the specific AI model, supports autocompletion via
     * {@link AutoCompleteDecorator}.
     */
    private JComboBox<AbstractModel> modelComboBox;
    /**
     * The button to open the global model registry viewer for deep exploration.
     */
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
     * Initializes the UI components using MigLayout and populates the model
     * selectors.
     */
    public void initComponents() {
        setLayout(new MigLayout("insets 5, fillx, gap 10",
                "[][][][]push[][][]", // Nickname, Save, Clone, Dispose, PUSH, Provider, Model, Search
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
        saveSessionButton = new JButton(new SaveIcon(ICON_SIZE));
        saveSessionButton.setToolTipText("Save Session");
        saveSessionButton.addActionListener(e -> saveSession());
        add(saveSessionButton);

        cloneSessionButton = new JButton(new uno.anahata.asi.swing.icons.CloneIcon(ICON_SIZE));
        cloneSessionButton.setToolTipText("Clone Session");
        cloneSessionButton.addActionListener(e -> cloneSession());
        add(cloneSessionButton);

        disposeSessionButton = new JButton(new uno.anahata.asi.swing.icons.DeleteIcon(ICON_SIZE));
        disposeSessionButton.setToolTipText("Dispose Session");
        disposeSessionButton.addActionListener(e -> disposeSession());
        add(disposeSessionButton);

        // Provider ComboBox (Right-aligned, skipping the push column)
        providerComboBox = new JComboBox<>();
        providerComboBox.setToolTipText("Select AI Provider");
        providerComboBox.setRenderer(new AiProviderRenderer());
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

        // --- Pre-selection Logic ---
        AbstractModel selectedAgiModel = agi.getSelectedModel();
        if (selectedAgiModel != null) {
            for (int i = 0; i < providerComboBox.getItemCount(); i++) {
                AbstractAiProvider p = providerComboBox.getItemAt(i);
                if (p.getProviderId().equals(selectedAgiModel.getProviderId())) {
                    providerComboBox.setSelectedItem(p);
                    break;
                }
            }
        } else if (providerComboBox.getItemCount() > 0) {
            providerComboBox.setSelectedIndex(0);
        }

        // Initial model load and synchronization
        updateModelsForSelectedProvider();

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
     * Fetches all registered providers from the agi session and adds them to
     * the combo box.
     */
    private void populateProviders() {
        List<AbstractAiProvider> providers = agi.getProviders();
        for (AbstractAiProvider provider : providers) {
            providerComboBox.addItem(provider);
        }
    }

    /**
     * Installs action listeners for provider and model selection.
     */
    private void addListeners() {
        new EdtPropertyChangeListener(this, agi, "nickname", evt -> {
            if (!java.util.Objects.equals(nicknameField.getText(), evt.getNewValue())) {
                nicknameField.setText((String) evt.getNewValue());
            }
        });
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
     * Opens the provider registry viewer dialog to search and select models
     * from all providers.
     * <p>This operation performs model discovery across all enabled providers 
     * in a background task to keep the UI responsive.</p>
     */
    private void showProviderRegistry() {
        new SwingTask<List<AbstractModel>>(agiPanel, "Collecting Models from Providers", () -> {
            return agi.getProviders().stream()
                    .filter(AbstractAiProvider::isEnabled)
                    .flatMap(provider -> provider.getModels().stream())
                    .collect(Collectors.toList());
        }, allModels -> {
            JDialog dialog = new JDialog(SwingUtilities.getWindowAncestor(this), "AI Provider & Model Registry", JDialog.ModalityType.MODELESS);

            AiProviderRegistryViewer viewer = new AiProviderRegistryViewer(allModels, selectedModel -> {
                dialog.dispose();
                // 1. Update domain model first so updateModelsForSelectedProvider picks it up
                agi.setSelectedModel(selectedModel);

                // 2. Find and select the corresponding provider in the UI
                for (int i = 0; i < providerComboBox.getItemCount(); i++) {
                    AbstractAiProvider p = providerComboBox.getItemAt(i);
                    if (p.getProviderId().equals(selectedModel.getProviderId())) {
                        providerComboBox.setSelectedItem(p);
                        // 3. Force a refresh of the models list and selection
                        updateModelsForSelectedProvider();
                        break;
                    }
                }
            });

            dialog.getContentPane().add(viewer);
            dialog.setPreferredSize(new Dimension(1200, 800));
            dialog.pack();
            dialog.setLocationRelativeTo(this);
            dialog.setVisible(true);
        }).start();
    }

    /**
     * Updates the model combo box items based on the currently selected
     * provider.
     * <p>This operation is performed asynchronously to avoid freezing the EDT 
     * while the provider fetches fresh models from its API.</p>
     */
    private void updateModelsForSelectedProvider() {
        AbstractAiProvider selectedProvider = (AbstractAiProvider) providerComboBox.getSelectedItem();
        
        if (selectedProvider != null) {
            // Visual feedback: disable selection while discovery is in progress
            modelComboBox.setEnabled(false);
            
            new SwingTask<List<? extends AbstractModel>>(agiPanel, "Discovering Models", () -> {
                return selectedProvider.getModels();
            }, models -> {
                DefaultComboBoxModel<AbstractModel> comboModel = new DefaultComboBoxModel<>();
                for (AbstractModel model : models) {
                    comboModel.addElement(model);
                }
                modelComboBox.setModel(comboModel);
                modelComboBox.setEnabled(true);
                
                // Restore selection from domain if it belongs to this provider
                AbstractModel currentAgiModel = agi.getSelectedModel();
                if (currentAgiModel != null && currentAgiModel.getProviderId().equals(selectedProvider.getProviderId())) {
                    for (int i = 0; i < modelComboBox.getItemCount(); i++) {
                        if (modelComboBox.getItemAt(i).getModelId().equals(currentAgiModel.getModelId())) {
                            modelComboBox.setSelectedIndex(i);
                            return;
                        }
                    }
                }
                
                // Fallback to first model if nothing selected
                if (modelComboBox.getItemCount() > 0 && (modelComboBox.getSelectedIndex() == -1 || agi.getSelectedModel() == null)) {
                    modelComboBox.setSelectedIndex(0);
                }
                
                // Explicitly sync back to domain to ensure the Agi session is aware of the final choice
                AbstractModel selected = (AbstractModel) modelComboBox.getSelectedItem();
                if (selected != null) {
                    agi.setSelectedModel(selected);
                }
            }, error -> {
                log.error("Failed to discover models for provider: {}", selectedProvider.getDisplayName(), error);
                modelComboBox.setEnabled(true);
            }, false).start();
        } else {
            modelComboBox.setModel(new DefaultComboBoxModel<>());
        }
    }

    /**
     * Triggers a manual save and exports the session to a .kryo file chosen by
     * the user.
     */
    private void saveSession() {
        new SwingTask<>(agiPanel, "Save Session", () -> {
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
                    new SwingTask<>(agiPanel, "Exporting Session", () -> {
                        try {
                            byte[] data = KryoUtils.serialize(agi);
                            Files.write(finalFile.toPath(), data);
                            log.info("Session exported successfully to: {}", finalFile.getAbsolutePath());
                        } catch (Exception ex) {
                            log.error("Failed to export session", ex);
                            throw ex;
                        }
                        return null;
                    }).start();
                }
            });
            return null;
        }).start();
    }

    /**
     * Clones the current session and opens it in a new tab.
     */
    private void cloneSession() {
        new SwingTask<>(agiPanel, "Clone Session", () -> {
            agi.getConfig().getAsiContainer().cloneSession(agi);
            return null;
        }).start();
    }

    /**
     * Disposes the current session.
     */
    private void disposeSession() {
        int result = JOptionPane.showConfirmDialog(this, 
            "Are you sure you want to dispose this session?\\nn"
                    + "If you ever need it back, you can import it from the 'diposed' sessions folder ", 
            "Dispose Session", JOptionPane.YES_NO_OPTION);

        if (result == JOptionPane.YES_OPTION) {
            agi.getConfig().getAsiContainer().dispose(agi);
        }
    }

}
