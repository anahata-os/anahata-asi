/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.io.File;
import java.nio.file.Files;
import java.util.List;
import java.util.stream.Collectors;
import javax.swing.*;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXTextField;
import org.jdesktop.swingx.autocomplete.AutoCompleteDecorator;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.internal.kryo.KryoUtils;
import uno.anahata.asi.model.provider.AbstractAiProvider;
import uno.anahata.asi.model.provider.AbstractModel;
import uno.anahata.asi.swing.icons.LoadSessionIcon;
import uno.anahata.asi.swing.icons.SaveSessionIcon;
import uno.anahata.asi.swing.icons.SearchIcon;
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.swing.provider.ProviderRegistryViewer;

/**
 * The header panel for the chat UI, containing the chat nickname, session controls,
 * and provider/model selection components.
 *
 * @author anahata
 */
@Slf4j
public class HeaderPanel extends JPanel {
    private static final int ICON_SIZE = 24;

    private final ChatPanel chatPanel;
    private Chat chat;

    private JXTextField nicknameField;
    private JButton saveSessionButton;
    private JButton loadSessionButton;
    private JComboBox<AbstractAiProvider> providerComboBox;
    private JComboBox<AbstractModel> modelComboBox;
    private JButton searchModelsButton;

    public HeaderPanel(ChatPanel chatPanel) {
        this.chatPanel = chatPanel;
        this.chat = chatPanel.getChat();
        log.info("Header Panel constructor, selected chat model: " + chat.getSelectedModel());        
    }

    public void initComponents() {
        setLayout(new MigLayout("insets 5, fillx, gap 10",
                                "[][][]push[][][]", // Component constraints: Nickname, Save, Load, PUSH, Provider, Model, Search
                                "[]")); // Row constraints

        // Nickname Field
        nicknameField = new JXTextField("Nickname");
        nicknameField.setText(chat.getNickname());
        nicknameField.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent e) {
                chat.setNickname(nicknameField.getText());
            }
        });
        add(nicknameField, "w 150!");

        // Session Buttons
        saveSessionButton = new JButton(new SaveSessionIcon(ICON_SIZE));
        saveSessionButton.setToolTipText("Save Session");
        saveSessionButton.addActionListener(e -> saveSession());
        add(saveSessionButton);

        loadSessionButton = new JButton(new LoadSessionIcon(ICON_SIZE));
        loadSessionButton.setToolTipText("Load Session");
        loadSessionButton.addActionListener(e -> loadSession());
        add(loadSessionButton);

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

        // Select the chat's currently selected model if available
        AbstractModel selectedChatModel = chat.getSelectedModel();
        System.out.println("Preselecting " + selectedChatModel);
        if (selectedChatModel != null) {
            // First, find and set the provider
            for (int i = 0; i < providerComboBox.getItemCount(); i++) {
                AbstractAiProvider provider = providerComboBox.getItemAt(i);
                if (provider.getProviderId().equals(selectedChatModel.getProviderId())) {
                    log.info("Preselecting provider " + provider);
                    providerComboBox.setSelectedItem(provider);
                    // Explicitly update models for the selected provider after setting the provider
                    updateModelsForSelectedProvider(); 
                    break;
                }
            }
            
            // Then, find and set the model
            for (int i = 0; i < modelComboBox.getItemCount(); i++) {
                if (modelComboBox.getItemAt(i).getModelId().equals(selectedChatModel.getModelId())) {
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
                chat.setSelectedModel((AbstractModel) modelComboBox.getSelectedItem());
            }
        }
        
        // Add listeners AFTER initial population and selection
        addListeners();
    }

    /**
     * Reloads the panel with the new chat state.
     */
    public void reload() {
        this.chat = chatPanel.getChat();
        removeAll();
        initComponents();
        revalidate();
        repaint();
    }

    private void populateProviders() {
        List<AbstractAiProvider> providers = chat.getProviders();
        for (AbstractAiProvider provider : providers) {
            providerComboBox.addItem(provider);
        }
    }

    private void addListeners() {
        providerComboBox.addActionListener(e -> updateModelsForSelectedProvider());
        
        modelComboBox.addActionListener(e -> {
            AbstractModel selectedModel = (AbstractModel) modelComboBox.getSelectedItem();
            if (selectedModel != null) {
                chat.setSelectedModel(selectedModel);
            }
        });
        
        searchModelsButton.addActionListener(e -> showProviderRegistry());
    }
    
    private void showProviderRegistry() {
        // Collect all models from all providers
        List<AbstractModel> allModels = chat.getProviders().stream()
            .flatMap(provider -> provider.getModels().stream())
            .collect(Collectors.toList());

        JDialog dialog = new JDialog(SwingUtilities.getWindowAncestor(this), "AI Provider & Model Registry", JDialog.ModalityType.MODELESS);
        
        ProviderRegistryViewer viewer = new ProviderRegistryViewer(allModels, selectedModel -> {
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

    private void updateModelsForSelectedProvider() {
        AbstractAiProvider selectedProvider = (AbstractAiProvider) providerComboBox.getSelectedItem();
        modelComboBox.removeAllItems();
        if (selectedProvider != null) {
            List<? extends AbstractModel> models = selectedProvider.getModels();
            for (AbstractModel model : models) {
                modelComboBox.addItem(model);
            }
        }
    }

    private void saveSession() {
        new SwingTask<>(this, "Save Session", () -> {
            chat.save();
            return null;
        }).execute();
    }

    private void loadSession() {
        JFileChooser fileChooser = new JFileChooser(chat.getConfig().getContainer().getSessionsDir().toFile());
        fileChooser.setDialogTitle("Load Chat Session");
        if (fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
            File file = fileChooser.getSelectedFile();
            new SwingTask<>(this, "Load Session", () -> {
                byte[] data = Files.readAllBytes(file.toPath());
                Chat loadedChat = KryoUtils.deserialize(data, Chat.class);
                loadedChat.rebind(chat.getConfig().getContainer());
                chat.getConfig().getContainer().register(loadedChat);
                return loadedChat;
            }, loadedChat -> {
                chatPanel.reload(loadedChat);
            }).execute();
        }
    }

    // Custom renderer to display provider's display name
    private static class ProviderRenderer extends DefaultListCellRenderer {
        @Override
        public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
            super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
            if (value instanceof AbstractAiProvider) {
                setText(((AbstractAiProvider) value).getProviderId());
            }
            return this;
        }
    }

    // Custom renderer to display model's display name
    private static class ModelRenderer extends DefaultListCellRenderer {
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
