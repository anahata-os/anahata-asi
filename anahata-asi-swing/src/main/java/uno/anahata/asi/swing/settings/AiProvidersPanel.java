/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.settings;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.ListCellRenderer;
import javax.swing.UIManager;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.AiProviderPanel;
import uno.anahata.asi.swing.icons.AddIcon;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * A dedicated, high-density AI Provider management panel.
 * <p>
 * Displays a left-hand vertical tabbed pane of all registered AI providers in the
 * container, allowing instant configuration of connectivity, API key pools, and
 * provider-specific model registries.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@Getter
public class AiProvidersPanel extends JPanel {

    /**
     * The parent Swing ASI container instance.
     */
    private final AbstractSwingAsiContainer container;

    /**
     * The vertical tabbed pane hosting individual provider panels.
     */
    private final JTabbedPane providerTabs;

    /**
     * The active provider panels currently displayed.
     */
    private final List<AiProviderPanel> activePanels = new ArrayList<>();

    /**
     * Constructs a new AiProvidersPanel for the given container.
     *
     * @param container The parent ASI container.
     */
    public AiProvidersPanel(@NonNull AbstractSwingAsiContainer container) {
        super(new BorderLayout());
        this.container = container;
        setOpaque(false);

        // Sidebar Header with Add Provider Action
        JPanel sidebarHeader = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 8));
        sidebarHeader.setOpaque(false);
        JButton addBtn = new JButton("Add New Provider", new AddIcon(16));
        addBtn.setToolTipText("Add a new AI Provider to this container");
        addBtn.addActionListener(e -> showAddProviderDialog());
        sidebarHeader.add(addBtn);

        this.providerTabs = new JTabbedPane(JTabbedPane.LEFT);
        providerTabs.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);

        refreshProviderTabs();

        JPanel leftWrapper = new JPanel(new BorderLayout());
        leftWrapper.setOpaque(false);
        leftWrapper.add(sidebarHeader, BorderLayout.NORTH);
        leftWrapper.add(providerTabs, BorderLayout.CENTER);

        add(leftWrapper, BorderLayout.CENTER);
    }

    /**
     * Refreshes the provider tabs from the container's registered providers.
     */
    public void refreshProviderTabs() {
        int previousIndex = providerTabs.getSelectedIndex();
        providerTabs.removeAll();
        activePanels.clear();

        for (AbstractAiProvider p : container.getAllProviders()) {
            AiProviderPanel panel = new AiProviderPanel(null, p, () -> removeProvider(p));
            Icon icon = IconUtils.getIcon("aiproviders/" + p.getClass().getName() + ".png", 16, 16);
            providerTabs.addTab(p.getDisplayName(), icon, panel);
            activePanels.add(panel);
        }

        if (previousIndex >= 0 && previousIndex < providerTabs.getTabCount()) {
            providerTabs.setSelectedIndex(previousIndex);
        } else if (providerTabs.getTabCount() > 0) {
            providerTabs.setSelectedIndex(0);
        }
    }

    /**
     * Displays a dialog allowing the user to instantiate and register a new AI provider.
     */
    private void showAddProviderDialog() {
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
                    refreshProviderTabs();
                    providerTabs.setSelectedIndex(providerTabs.getTabCount() - 1);
                } catch (Exception ex) {
                    log.error("Failed to instantiate and register provider", ex);
                    JOptionPane.showMessageDialog(this, "Failed to create provider: " + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                }
            }
        }
    }

    /**
     * Resolves the human-readable description for a provider class.
     *
     * @param clazz The provider class.
     * @return Description text.
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
     * Removes and deletes a provider from the container.
     *
     * @param provider The provider to remove.
     */
    private void removeProvider(AbstractAiProvider provider) {
        String name = provider.getDisplayName();
        int choice = JOptionPane.showConfirmDialog(this,
                "Are you sure you want to remove the provider '" + name + "'?\n\n"
                + "This will unregister the provider and delete its configuration file.",
                "Remove Provider", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

        if (choice == JOptionPane.YES_OPTION) {
            try {
                provider.remove();
                container.unregisterProvider(provider.getUuid());
                refreshProviderTabs();
            } catch (IOException ex) {
                log.error("Failed to remove provider: {}", provider.getUuid(), ex);
                JOptionPane.showMessageDialog(this, "Failed to delete provider file: " + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
            }
        }
    }

    /**
     * A structured representation of an available AI provider type.
     *
     * @param clazz The concrete provider class.
     * @param displayName The user-friendly name.
     * @param description Brief explanation.
     * @param icon Visual brand icon.
     */
    private record ProviderItem(
            Class<? extends AbstractAiProvider> clazz,
            String displayName,
            String description,
            Icon icon
    ) {
        @Override
        public String toString() {
            return displayName;
        }
    }

    /**
     * High-fidelity cell renderer for displaying rich provider information in selection dialogs.
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

        /** Constructs the renderer. */
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
