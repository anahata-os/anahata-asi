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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
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
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.icons.AddIcon;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.icons.RestartIcon;
import uno.anahata.asi.swing.icons.SearchIcon;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;

/**
 * A dedicated master-detail management panel for AGI Session Templates.
 * <p>
 * Displays a list of all stored AGI templates on the left sidebar and hosts the full,
 * interactive {@link AgiPanel} for the selected template on the right side.
 * Allows users to create, clone, customize, delete, set default, and launch active sessions
 * directly from templates.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@Getter
public class TemplatesPanel extends JPanel {

    /**
     * The parent Swing ASI container instance.
     */
    private final AbstractSwingAsiContainer container;

    /**
     * The list model backing the sidebar template list.
     */
    private final DefaultListModel<Agi> listModel;

    /**
     * The visual sidebar list component displaying registered AGI templates.
     */
    private final JList<Agi> templateList;

    /**
     * Container panel holding the active template's embedded {@link AgiPanel}.
     */
    private final JPanel detailContainer;

    /**
     * Cache of initialized AgiPanel instances keyed by template session ID.
     */
    private final Map<String, AgiPanel> cachedPanels = new HashMap<>();

    /**
     * The currently selected template Agi instance.
     */
    private Agi selectedTemplate;

    /**
     * Button to designate the currently selected template as the default bootstrap template.
     */
    private final JButton makeDefaultButton;

    /**
     * Button to delete/dispose the selected template.
     */
    private final JButton deleteButton;

    /**
     * Button to spawn an active AGI session from the selected template.
     */
    private final JButton launchButton;

    /**
     * Constructs a new Master-Detail TemplatesPanel bound to the specified container.
     *
     * @param container The parent ASI container instance.
     */
    public TemplatesPanel(@NonNull AbstractSwingAsiContainer container) {
        super(new BorderLayout());
        this.container = container;
        setOpaque(false);

        listModel = new DefaultListModel<>();
        templateList = new JList<>(listModel);
        templateList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        templateList.setCellRenderer(new TemplateListCellRenderer(container));

        // --- Left Sidebar (WEST) ---
        JPanel sidebar = new JPanel(new BorderLayout());
        sidebar.setPreferredSize(new Dimension(260, -1));
        sidebar.setBorder(BorderFactory.createMatteBorder(0, 0, 0, 1, new Color(200, 200, 200)));

        // Sidebar Top: Add Button
        JPanel sidebarHeader = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 8));
        sidebarHeader.setOpaque(false);
        JButton addBtn = new JButton("Add New Template", new AddIcon(16));
        addBtn.setToolTipText("Create a fresh AGI template");
        addBtn.addActionListener(e -> showCreateTemplateDialog());
        sidebarHeader.add(addBtn);
        sidebar.add(sidebarHeader, BorderLayout.NORTH);

        // Sidebar Center: Template List
        JScrollPane listScroll = new JScrollPane(templateList);
        listScroll.setBorder(null);
        sidebar.add(listScroll, BorderLayout.CENTER);

        // Sidebar Bottom: Action Toolbar
        JPanel actionsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 4, 4));
        actionsPanel.setOpaque(false);

        makeDefaultButton = new JButton("Set Default", new RestartIcon(14));
        makeDefaultButton.setToolTipText("Set this template as default.kryo (bootstraps 'New AGI')");
        makeDefaultButton.setEnabled(false);
        makeDefaultButton.addActionListener(e -> setAsDefault());
        actionsPanel.add(makeDefaultButton);

        deleteButton = new JButton("Delete", new DeleteIcon(14));
        deleteButton.setToolTipText("Delete the selected template");
        deleteButton.setEnabled(false);
        deleteButton.addActionListener(e -> deleteSelectedTemplate());
        actionsPanel.add(deleteButton);

        launchButton = new JButton("Launch", new SearchIcon(14));
        launchButton.setToolTipText("Spawn an active AGI session from this template");
        launchButton.setEnabled(false);
        launchButton.addActionListener(e -> launchFromSelectedTemplate());
        actionsPanel.add(launchButton);

        sidebar.add(actionsPanel, BorderLayout.SOUTH);
        add(sidebar, BorderLayout.WEST);

        // --- Center Detail Area (CENTER) ---
        detailContainer = new JPanel(new BorderLayout());
        detailContainer.setOpaque(false);
        add(detailContainer, BorderLayout.CENTER);

        // Initial list load
        refreshTemplateList();

        // Template selection listener
        templateList.addListSelectionListener(e -> {
            if (!e.getValueIsAdjusting()) {
                Agi selected = templateList.getSelectedValue();
                if (selected != selectedTemplate) {
                    selectedTemplate = selected;
                    updateDetailPanel(selected);
                    updateActionButtonsState();
                }
            }
        });

        // Reactive listener for container template registry changes
        new EdtPropertyChangeListener(this, container, "templates", evt -> refreshTemplateList());
    }

    /**
     * Refreshes the sidebar template list from the container's registered templates,
     * maintaining the active selection if still valid.
     */
    public void refreshTemplateList() {
        listModel.clear();
        List<Agi> templates = container.getTemplates();
        for (Agi t : templates) {
            listModel.addElement(t);
        }

        // Clean up cached panels for templates no longer in memory
        cachedPanels.keySet().removeIf(id -> templates.stream().noneMatch(t -> t.getConfig().getSessionId().equals(id)));

        if (selectedTemplate != null && listModel.contains(selectedTemplate)) {
            templateList.setSelectedValue(selectedTemplate, true);
        } else if (!listModel.isEmpty()) {
            selectedTemplate = listModel.get(0);
            templateList.setSelectedValue(selectedTemplate, true);
            updateDetailPanel(selectedTemplate);
        } else {
            selectedTemplate = null;
            updateDetailPanel(null);
        }
        updateActionButtonsState();
    }

    /**
     * Updates the center detail panel with the embedded {@link AgiPanel} for the selected template.
     *
     * @param template The template Agi instance to display, or null if no selection.
     */
    private void updateDetailPanel(Agi template) {
        detailContainer.removeAll();
        if (template != null) {
            String templateId = template.getConfig().getSessionId();
            AgiPanel panel = cachedPanels.computeIfAbsent(templateId, id -> {
                AgiPanel p = new AgiPanel(template);
                p.initComponents();
                return p;
            });
            detailContainer.add(panel, BorderLayout.CENTER);
        } else {
            JPanel emptyPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 20));
            emptyPanel.setOpaque(false);
            JLabel label = new JLabel("No AGI template selected. Create a new template to begin.");
            label.setFont(label.getFont().deriveFont(Font.ITALIC, 13f));
            label.setForeground(UIManager.getColor("Label.disabledForeground"));
            emptyPanel.add(label);
            detailContainer.add(emptyPanel, BorderLayout.CENTER);
        }
        detailContainer.revalidate();
        detailContainer.repaint();
    }

    /**
     * Updates the enabled state of sidebar action buttons based on the current selection.
     */
    private void updateActionButtonsState() {
        boolean hasSelection = selectedTemplate != null;
        deleteButton.setEnabled(hasSelection);
        launchButton.setEnabled(hasSelection);

        if (hasSelection) {
            boolean isDefault = "default".equalsIgnoreCase(selectedTemplate.getConfig().getSessionId());
            makeDefaultButton.setEnabled(!isDefault);
        } else {
            makeDefaultButton.setEnabled(false);
        }
    }

    /**
     * Displays a dialog prompting the user to name and create a new template.
     */
    private void showCreateTemplateDialog() {
        String templateId = JOptionPane.showInputDialog(this,
                "Enter a unique ID for the new AGI template (e.g. 'python-dev', 'code-reviewer'):",
                "New AGI Template", JOptionPane.PLAIN_MESSAGE);

        if (templateId != null && !templateId.trim().isEmpty()) {
            templateId = templateId.trim();
            final String finalId = templateId;
            boolean exists = container.getTemplates().stream()
                    .anyMatch(t -> t.getConfig().getSessionId().equalsIgnoreCase(finalId));
            if (exists) {
                JOptionPane.showMessageDialog(this,
                        "A template with ID '" + templateId + "' already exists.",
                        "Template Exists", JOptionPane.WARNING_MESSAGE);
                return;
            }

            try {
                Agi created = container.createTemplate(templateId);
                refreshTemplateList();
                templateList.setSelectedValue(created, true);
            } catch (IOException ex) {
                log.error("Failed to create template: {}", templateId, ex);
                JOptionPane.showMessageDialog(this,
                        "Failed to create template: " + ex.getMessage(),
                        "Error", JOptionPane.ERROR_MESSAGE);
            }
        }
    }

    /**
     * Sets the currently selected template as the default template ("default.kryo")
     * and disposes the original template to prevent duplicates.
     */
    private void setAsDefault() {
        if (selectedTemplate == null) {
            return;
        }

        String sourceId = selectedTemplate.getConfig().getSessionId();
        int confirm = JOptionPane.showConfirmDialog(this,
                "Set template '" + sourceId + "' as the default template?\n\n"
                + "This will overwrite 'default.kryo' and archive '" + sourceId + "' to avoid duplicates.\n"
                + "Any future 'New AGI' sessions will bootstrap from this template.",
                "Set as Default Template", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

        if (confirm == JOptionPane.YES_OPTION) {
            try {
                Agi toDispose = selectedTemplate;
                Agi defaultAgi = container.createTemplateFromSession(toDispose, "default");
                container.dispose(toDispose);
                refreshTemplateList();
                templateList.setSelectedValue(defaultAgi, true);
            } catch (IOException ex) {
                log.error("Failed to set template as default", ex);
                JOptionPane.showMessageDialog(this,
                        "Failed to set default template: " + ex.getMessage(),
                        "Error", JOptionPane.ERROR_MESSAGE);
            }
        }
    }

    /**
     * Prompts for confirmation and permanently disposes/deletes the selected template.
     */
    private void deleteSelectedTemplate() {
        if (selectedTemplate == null) {
            return;
        }

        String id = selectedTemplate.getConfig().getSessionId();
        int confirm = JOptionPane.showConfirmDialog(this,
                "Are you sure you want to delete template '" + id + "'?\n\n"
                + "It will be safely archived to the 'templates/disposed' directory.",
                "Delete Template", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

        if (confirm == JOptionPane.YES_OPTION) {
            container.dispose(selectedTemplate);
            refreshTemplateList();
        }
    }

    /**
     * Creates and opens a new active session instantiated directly from the selected template.
     */
    private void launchFromSelectedTemplate() {
        if (selectedTemplate == null) {
            return;
        }
        container.createNewAgiFromTemplate(selectedTemplate);
    }

    /**
     * High-fidelity cell renderer for displaying templates in the sidebar list.
     */
    private static class TemplateListCellRenderer extends JPanel implements ListCellRenderer<Agi> {

        /**
         * Label for the template icon.
         */
        private final JLabel iconLabel = new JLabel();

        /**
         * Label for the template ID and default badge.
         */
        private final JLabel idLabel = new JLabel();

        /**
         * Label for the nickname and model info.
         */
        private final JLabel detailsLabel = new JLabel();

        /**
         * The active container to check default template status.
         */
        private final AbstractSwingAsiContainer container;

        /**
         * Constructs the cell renderer.
         *
         * @param container The active container instance.
         */
        public TemplateListCellRenderer(AbstractSwingAsiContainer container) {
            this.container = container;
            setLayout(new GridBagLayout());
            setOpaque(true);
            setBorder(BorderFactory.createEmptyBorder(6, 8, 6, 8));

            idLabel.setFont(idLabel.getFont().deriveFont(Font.BOLD, 12f));
            detailsLabel.setFont(detailsLabel.getFont().deriveFont(Font.PLAIN, 11f));
            detailsLabel.setForeground(UIManager.getColor("Label.disabledForeground"));

            GridBagConstraints gbc = new GridBagConstraints();
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.gridheight = 2;
            gbc.insets = new Insets(0, 0, 0, 8);
            gbc.anchor = GridBagConstraints.CENTER;
            add(iconLabel, gbc);

            gbc.gridx = 1;
            gbc.gridy = 0;
            gbc.gridheight = 1;
            gbc.insets = new Insets(0, 0, 2, 0);
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.weightx = 1.0;
            add(idLabel, gbc);

            gbc.gridx = 1;
            gbc.gridy = 1;
            gbc.gridheight = 1;
            gbc.insets = new Insets(0, 0, 0, 0);
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.weightx = 1.0;
            add(detailsLabel, gbc);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Component getListCellRendererComponent(
                JList<? extends Agi> list,
                Agi value,
                int index,
                boolean isSelected,
                boolean cellHasFocus
        ) {
            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
                idLabel.setForeground(list.getSelectionForeground());
                detailsLabel.setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
                idLabel.setForeground(list.getForeground());
                detailsLabel.setForeground(UIManager.getColor("Label.disabledForeground"));
            }

            if (value != null) {
                String id = value.getConfig().getSessionId();
                boolean isDefault = "default".equalsIgnoreCase(id);

                if (isDefault) {
                    idLabel.setText("⭐ " + id + " [DEFAULT]");
                } else {
                    idLabel.setText(id);
                }

                String nick = value.getNickname();
                String model = value.getConfig().getSelectedModelId();
                String details = "";
                if (nick != null && !nick.isBlank() && !nick.equalsIgnoreCase(id)) {
                    details += nick + " • ";
                }
                details += (model != null && !model.isBlank()) ? model : "Default Model";
                detailsLabel.setText(details);

                iconLabel.setIcon(IconUtils.getIcon("v2/anahata.png", 20, 20));
            }
            return this;
        }
    }
}
