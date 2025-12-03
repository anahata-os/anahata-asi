/*
 * Licensed under the Anahata Software License (AS IS) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.util.List;
import java.util.stream.Collectors;
import javax.swing.*;
import lombok.Getter;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXTextField;
import org.jdesktop.swingx.autocomplete.AutoCompleteDecorator;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.provider.AbstractAiProvider;
import uno.anahata.ai.model.provider.AbstractModel;
import uno.anahata.ai.swing.LoadSessionIcon;
import uno.anahata.ai.swing.SaveSessionIcon;
import uno.anahata.ai.swing.SearchIcon;
import uno.anahata.ai.swing.provider.ProviderRegistryViewer;

/**
 * The header panel for the chat UI, containing the chat nickname, session controls,
 * and provider/model selection components.
 *
 * @author pablo
 */
@Getter
public class HeaderPanel extends JPanel {
    private static final int ICON_SIZE = 24;

    private final Chat chat;

    private JXTextField nicknameField;
    private JButton saveSessionButton;
    private JButton loadSessionButton;
    private JComboBox<AbstractAiProvider> providerComboBox;
    private JComboBox<AbstractModel> modelComboBox;
    private JButton searchModelsButton;

    public HeaderPanel(Chat chat) {
        this.chat = chat;
    }

    public void initComponents() {
        setLayout(new MigLayout("insets 5, fillx, gap 10",
                                "[][][][]push[][][]", // Component constraints
                                "[]")); // Row constraints

        // Nickname Field
        nicknameField = new JXTextField("Nickname");
        nicknameField.setText(chat.getConfig().getName());
        nicknameField.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent e) {
                chat.getConfig().setName(nicknameField.getText());
            }
        });
        add(nicknameField, "w 150!");

        // Session Buttons
        saveSessionButton = new JButton(new SaveSessionIcon(ICON_SIZE));
        saveSessionButton.setToolTipText("Save Session");
        saveSessionButton.addActionListener(e -> {
            // TODO: Implement save session logic
        });
        add(saveSessionButton);

        loadSessionButton = new JButton(new LoadSessionIcon(ICON_SIZE));
        loadSessionButton.setToolTipText("Load Session");
        loadSessionButton.addActionListener(e -> {
            // TODO: Implement load session logic
        });
        add(loadSessionButton);

        // Provider ComboBox
        providerComboBox = new JComboBox<>();
        providerComboBox.setToolTipText("Select AI Provider");
        providerComboBox.setRenderer(new ProviderRenderer());
        add(providerComboBox, "w 150!");

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
        
        // Populate and set listeners
        populateProviders();
        addListeners();

        // Set initial selection
        if (providerComboBox.getItemCount() > 0) {
            providerComboBox.setSelectedIndex(0);
            updateModelsForSelectedProvider();
            if (modelComboBox.getItemCount() > 0) {
                modelComboBox.setSelectedIndex(0);
                chat.setSelectedModel((AbstractModel) modelComboBox.getSelectedItem());
            }
        }
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

        ProviderRegistryViewer viewer = new ProviderRegistryViewer(allModels);
        
        JDialog dialog = new JDialog(SwingUtilities.getWindowAncestor(this), "AI Provider & Model Registry", JDialog.ModalityType.MODELESS);
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
