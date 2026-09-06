/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.settings;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.io.IOException;
import java.util.List;
import java.util.UUID;
import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.UIManager;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.AiProviderPanel;
import uno.anahata.asi.swing.icons.AddIcon;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.provider.AiProviderRenderer;

/**
 * A dedicated master-detail management panel for AI Providers in the ASI Container.
 * <p>
 * Implements a high-efficiency Master-Detail architecture consisting of a left-hand
 * sidebar list of all registered providers and exactly one reusable {@link AiProviderPanel}
 * on the right side. This design drastically reduces component overhead by reusing a single
 * form and table instance across all provider switches.
 * </p>
 * <p>
 * <b>Dirty Tracking:</b> Features integrated unsaved changes detection ({@link #checkUnsavedChanges()}),
 * automatically intercepting provider switches or dialog closes to prompt the user to save or discard
 * pending modifications.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@Getter
public class AiProvidersPanel extends JPanel {

    /**
     * The parent Swing ASI container instance managing global providers and thread pools.
     */
    private final AbstractSwingAsiContainer container;

    /**
     * The list model backing the sidebar provider selection list.
     */
    private final DefaultListModel<AbstractAiProvider> listModel;

    /**
     * The visual sidebar list component displaying registered AI providers.
     */
    private final JList<AbstractAiProvider> providerList;

    /**
     * Visual container panel holding the active typed detail panel.
     */
    private final JPanel detailPanelContainer;

    /**
     * The single active detail panel instance hosting provider forms and model tables.
     */
    private AiProviderPanel detailPanel;

    /**
     * The currently selected and displayed AI provider instance.
     */
    private AbstractAiProvider currentProvider;

    /**
     * Constructs a new Master-Detail AiProvidersPanel bound to the specified container.
     * <p>
     * Initializes the left sidebar with provider icons and compact add controls,
     * wires the dynamic {@link AiProviderPanel} in the center, and configures
     * reactive selection listeners with dirty checking.
     * </p>
     *
     * @param container The parent ASI container instance.
     */
    public AiProvidersPanel(@NonNull AbstractSwingAsiContainer container) {
        super(new BorderLayout());
        this.container = container;
        setOpaque(false);

        listModel = new DefaultListModel<>();
        providerList = new JList<>(listModel);
        providerList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        providerList.setCellRenderer(new AiProviderRenderer());

        // Left Sidebar (WEST)
        JPanel sidebar = new JPanel(new BorderLayout());
        sidebar.setPreferredSize(new Dimension(230, -1));
        sidebar.setBorder(BorderFactory.createMatteBorder(0, 0, 0, 1, new Color(200, 200, 200)));

        JPanel sidebarHeader = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 8));
        sidebarHeader.setOpaque(false);
        JButton addBtn = new JButton("Add New Provider", new AddIcon(16));
        addBtn.setToolTipText("Add a new AI Provider to this container");
        addBtn.addActionListener(e -> showAddProviderDialog());
        sidebarHeader.add(addBtn);
        sidebar.add(sidebarHeader, BorderLayout.NORTH);

        JScrollPane listScroll = new JScrollPane(providerList);
        listScroll.setBorder(null);
        sidebar.add(listScroll, BorderLayout.CENTER);

        add(sidebar, BorderLayout.WEST);

        // Dynamic Detail Container (CENTER)
        detailPanelContainer = new JPanel(new BorderLayout());
        detailPanelContainer.setOpaque(false);
        add(detailPanelContainer, BorderLayout.CENTER);

        List<AbstractAiProvider> all = container.getAllProviders();
        currentProvider = !all.isEmpty() ? all.get(0) : null;
        updateDetailPanel(currentProvider);

        refreshProviderList();

        providerList.addListSelectionListener(e -> {
            if (!e.getValueIsAdjusting()) {
                AbstractAiProvider selected = providerList.getSelectedValue();
                if (selected != null && selected != currentProvider) {
                    if (checkUnsavedChanges()) {
                        currentProvider = selected;
                        updateDetailPanel(selected);
                    } else {
                        providerList.setSelectedValue(currentProvider, false);
                    }
                }
            }
        });
    }

    /**
     * Updates the center detail panel with the appropriate typed panel for the given provider.
     *
     * @param provider The active provider entity to display.
     */
    private void updateDetailPanel(AbstractAiProvider provider) {
        detailPanelContainer.removeAll();
        if (provider != null) {
            this.detailPanel = uno.anahata.asi.swing.provider.AiProviderUiRegistry.getInstance()
                    .createPanel(container, provider, () -> removeCurrentProvider());
            detailPanelContainer.add(detailPanel, BorderLayout.CENTER);
        } else {
            this.detailPanel = null;
        }
        detailPanelContainer.revalidate();
        detailPanelContainer.repaint();
    }

    /**
     * Refreshes the sidebar provider list from the container's registered providers,
     * preserving the current selection if still valid.
     */
    public void refreshProviderList() {
        listModel.clear();
        for (AbstractAiProvider p : container.getAllProviders()) {
            listModel.addElement(p);
        }
        if (currentProvider != null && listModel.contains(currentProvider)) {
            providerList.setSelectedValue(currentProvider, true);
        } else if (!listModel.isEmpty()) {
            currentProvider = listModel.get(0);
            providerList.setSelectedValue(currentProvider, true);
            updateDetailPanel(currentProvider);
        }
    }

    /**
     * Evaluates whether the currently displayed provider panel has unsaved modifications
     * and prompts the user with a confirmation dialog if dirty.
     *
     * @return {@code true} if the operation can proceed (changes saved, discarded, or not modified);
     *         {@code false} if the user cancelled the transition.
     */
    public boolean checkUnsavedChanges() {
        if (detailPanel != null && detailPanel.isModified() && currentProvider != null) {
            int choice = JOptionPane.showConfirmDialog(this,
                    "You have unsaved changes for provider '" + currentProvider.getDisplayName() + "'.\n\nWould you like to save them before proceeding?",
                    "Unsaved Changes", JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.WARNING_MESSAGE);
            if (choice == JOptionPane.YES_OPTION) {
                try {
                    detailPanel.syncToProvider();
                    currentProvider.persist();
                    return true;
                } catch (IOException ex) {
                    log.error("Failed to save provider", ex);
                    JOptionPane.showMessageDialog(this, "Failed to save: " + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            } else if (choice == JOptionPane.NO_OPTION) {
                return true;
            } else {
                return false;
            }
        }
        return true;
    }

    /**
     * Displays a rich modal selection dialog allowing the user to instantiate, configure,
     * and persist a new AI provider entity.
     */
    private void showAddProviderDialog() {
        if (!checkUnsavedChanges()) {
            return;
        }
        List<Class<? extends AbstractAiProvider>> classes = AbstractSwingAsiContainer.AVAILABLE_PROVIDER_CLASSES;
        JComboBox<ProviderItem> combo = new JComboBox<>();
        for (Class<? extends AbstractAiProvider> clazz : classes) {
            String dispName = clazz.getSimpleName();
            try {
                AbstractAiProvider temp = clazz.getDeclaredConstructor().newInstance();
                dispName = temp.getDisplayName();
            } catch (Exception ignored) {
            }
            String desc = getProviderDescription(clazz);
            Icon icon = IconUtils.getIcon("aiproviders/" + clazz.getName() + ".png", 24, 24);
            combo.addItem(new ProviderItem(clazz, dispName, desc, icon));
        }

        combo.setRenderer(new ProviderListCellRenderer());

        int result = JOptionPane.showConfirmDialog(this, combo, "Select Provider Type", JOptionPane.OK_CANCEL_OPTION);
        if (result == JOptionPane.OK_OPTION) {
            ProviderItem selected = (ProviderItem) combo.getSelectedItem();
            if (selected != null) {
                try {
                    AbstractAiProvider newProvider = selected.clazz().getDeclaredConstructor().newInstance();
                    newProvider.setUuid(UUID.randomUUID().toString());
                    newProvider.setAsiContainer(container);
                    newProvider.persist();
                    container.registerProvider(newProvider);
                    currentProvider = newProvider;
                    refreshProviderList();
                    updateDetailPanel(newProvider);
                } catch (Exception ex) {
                    log.error("Failed to instantiate and register provider", ex);
                    JOptionPane.showMessageDialog(this, "Failed to create provider: " + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                }
            }
        }
    }

    /**
     * Resolves the human-readable description for an AI provider class.
     *
     * @param clazz The provider class to inspect.
     * @return The descriptive text string.
     */
    private static String getProviderDescription(Class<? extends AbstractAiProvider> clazz) {
        try {
            AbstractAiProvider temp = clazz.getDeclaredConstructor().newInstance();
            String desc = temp.getDescription();
            if (desc != null && !desc.isBlank()) {
                return desc;
            }
        } catch (Exception ignored) {
        }
        return "Custom AI model provider implementation.";
    }

    /**
     * Confirms with the user and permanently removes the currently selected provider entity,
     * deleting its {@code .kryo} storage file and unregistering it from the container.
     */
    private void removeCurrentProvider() {
        if (currentProvider == null) {
            return;
        }
        String name = currentProvider.getDisplayName();
        int choice = JOptionPane.showConfirmDialog(this,
                "Are you sure you want to remove the provider '" + name + "'?\n\n"
                + "This will unregister the provider and delete its configuration file.",
                "Remove Provider", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

        if (choice == JOptionPane.YES_OPTION) {
            try {
                currentProvider.remove();
                container.unregisterProvider(currentProvider.getUuid());
                currentProvider = null;
                refreshProviderList();
            } catch (IOException ex) {
                log.error("Failed to remove provider: {}", currentProvider.getUuid(), ex);
                JOptionPane.showMessageDialog(this, "Failed to delete provider file: " + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
            }
        }
    }

    /**
     * A structured descriptor record for an available AI provider type in creation dialogs.
     *
     * @param clazz The concrete provider implementation class.
     * @param displayName The human-readable display name.
     * @param description Brief summary of provider capabilities.
     * @param icon Visual brand icon.
     */
    private record ProviderItem(
            Class<? extends AbstractAiProvider> clazz,
            String displayName,
            String description,
            Icon icon
    ) {
        /**
         * {@inheritDoc}
         * <p>Returns the display name for default rendering.</p>
         */
        @Override
        public String toString() {
            return displayName;
        }
    }

    /**
     * High-fidelity cell renderer for displaying rich provider information in provider selection dialogs.
     */
    private static class ProviderListCellRenderer extends JPanel implements ListCellRenderer<ProviderItem> {

        /** The visual label for displaying the provider icon. */
        private final JLabel iconLabel = new JLabel();
        /** The visual label for displaying the provider's display name. */
        private final JLabel nameLabel = new JLabel();
        /** The visual label for displaying the class FQN. */
        private final JLabel fqnLabel = new JLabel();
        /** The visual label for displaying the provider description. */
        private final JLabel descLabel = new JLabel();

        /**
         * Constructs a new rich provider list cell renderer.
         */
        public ProviderListCellRenderer() {
            setLayout(new GridBagLayout());
            setOpaque(true);
            setBorder(BorderFactory.createEmptyBorder(4, 8, 4, 8));

            nameLabel.setFont(nameLabel.getFont().deriveFont(Font.BOLD, 12f));
            fqnLabel.setFont(fqnLabel.getFont().deriveFont(Font.ITALIC, 11f));
            fqnLabel.setForeground(UIManager.getColor("Label.disabledForeground"));
            descLabel.setFont(descLabel.getFont().deriveFont(11f));
            descLabel.setForeground(UIManager.getColor("Label.disabledForeground"));

            GridBagConstraints gbc = new GridBagConstraints();
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.gridheight = 3;
            gbc.insets = new Insets(0, 0, 0, 12);
            gbc.anchor = GridBagConstraints.CENTER;
            add(iconLabel, gbc);

            gbc.gridx = 1;
            gbc.gridy = 0;
            gbc.gridheight = 1;
            gbc.insets = new Insets(0, 0, 2, 0);
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.weightx = 1.0;
            add(nameLabel, gbc);

            gbc.gridx = 1;
            gbc.gridy = 1;
            gbc.gridheight = 1;
            gbc.insets = new Insets(0, 0, 2, 0);
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.weightx = 1.0;
            add(fqnLabel, gbc);

            gbc.gridx = 1;
            gbc.gridy = 2;
            gbc.gridheight = 1;
            gbc.insets = new Insets(0, 0, 0, 0);
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.weightx = 1.0;
            add(descLabel, gbc);
        }

        /**
         * {@inheritDoc}
         * <p>Renders provider icon, bold display name, italic FQN, and description text.</p>
         */
        @Override
        public Component getListCellRendererComponent(
                JList<? extends ProviderItem> list,
                ProviderItem value,
                int index,
                boolean isSelected,
                boolean cellHasFocus
        ) {
            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
                nameLabel.setForeground(list.getSelectionForeground());
                fqnLabel.setForeground(list.getSelectionForeground());
                descLabel.setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
                nameLabel.setForeground(list.getForeground());
                fqnLabel.setForeground(UIManager.getColor("Label.disabledForeground"));
                descLabel.setForeground(UIManager.getColor("Label.disabledForeground"));
            }

            if (value != null) {
                nameLabel.setText(value.displayName());
                fqnLabel.setText(value.clazz().getName());
                descLabel.setText(value.description());
                iconLabel.setIcon(value.icon());
            }
            return this;
        }
    }
}
