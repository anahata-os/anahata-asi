/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.provider;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import uno.anahata.asi.swing.components.WrapLayout;
import java.awt.KeyboardFocusManager;
import java.awt.Point;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import java.util.stream.Collectors;
import javax.swing.AbstractCellEditor;
import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.RowFilter;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXTable;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.ResponseModality;
import uno.anahata.asi.swing.components.EnumSetTableCellEditor;
import uno.anahata.asi.swing.icons.AddIcon;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.icons.NewIcon;
import uno.anahata.asi.swing.icons.RestartIcon;
import uno.anahata.asi.swing.internal.AnyChangeDocumentListener;
import uno.anahata.asi.swing.internal.SwingTask;

/**
 * A high-fidelity visual registry for exploring, configuring, and registering AI models.
 * <p>
 * This panel utilizes a {@link org.jdesktop.swingx.JXTable} and rich filter controls
 * (provider dropdown with icons, keyword/regex query field, response modality toggles,
 * and effectively-enabled state filter) to provide advanced discovery and lifecycle management.
 * Models can be enabled/disabled, added from API discovery caches to local persistent storage,
 * deleted, or reset when discrepancies occur.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class AiModelsPanel extends JPanel {

    /**
     * The advanced SwingX table instance for model discovery.
     */
    private final JXTable table;

    /**
     * The technical data model powering the table.
     */
    private final AiModelTableModel tableModel;

    /**
     * The real-time search and filter input field.
     */
    private final JTextField filterField;

    /**
     * The label for the provider combo box.
     */
    private final JLabel providerLabel;

    /**
     * The dropdown combo box for selecting/filtering by AI provider.
     */
    private final JComboBox<AbstractAiProvider> providerComboBox;

    /**
     * Toggle button for filtering by TEXT response modality.
     */
    private final JToggleButton textToggle;

    /**
     * Toggle button for filtering by IMAGE response modality.
     */
    private final JToggleButton imageToggle;

    /**
     * Toggle button for filtering by AUDIO response modality.
     */
    private final JToggleButton audioToggle;

    /**
     * Toggle button for filtering by VIDEO response modality.
     */
    private final JToggleButton videoToggle;

    /**
     * Checkbox to filter models to only effectively enabled providers.
     */
    private final JCheckBox effectivelyEnabledCheckbox;

    /**
     * Refresh button to reload models from providers' live APIs.
     */
    private final JButton refreshButton;

    /**
     * Button to add all unregistered API models to the local database in a single batch.
     */
    private final JButton addNewModelsButton;

    /**
     * Button to remove selected models from local storage.
     */
    private final JButton removeSelectedButton;

    /**
     * Button to reset selected models with discrepancies back to API specifications.
     */
    private final JButton resetSelectedButton;

    /**
     * Bottom status bar label showing active model counts and operations feedback.
     */
    private final JLabel statusLabel;

    /**
     * Bottom status bar indeterminate progress bar for async background tasks.
     */
    private final JProgressBar progressBar;

    /**
     * Container reference for background model fetching and executor pooling.
     */
    private final AbstractAsiContainer asiContainer;

    /**
     * Reactive callback for notifying the system of a user's model selection.
     */
    private final Consumer<AbstractModel> modelSelectionCallback;

    /**
     * Target provider lock when embedded in a single provider configuration panel.
     */
    private AbstractAiProvider targetProvider;

    /**
     * Constructs a new AiModelsPanel with full search, filter, and selection capabilities.
     * 
     * @param models The initial list of models to display.
     * @param modelSelectionCallback A callback for when a model is double-clicked.
     */
    public AiModelsPanel(List<AbstractModel> models, Consumer<AbstractModel> modelSelectionCallback) {
        this(models, null, modelSelectionCallback);
    }

    /**
     * Constructs a new AiModelsPanel with container-aware refresh capabilities.
     *
     * @param models The initial list of models to display.
     * @param asiContainer The parent ASI container providing thread pools and
     * provider registries.
     * @param modelSelectionCallback A callback for when a model is
     * double-clicked.
     */
    public AiModelsPanel(List<AbstractModel> models, AbstractAsiContainer asiContainer, Consumer<AbstractModel> modelSelectionCallback) {
        super(new BorderLayout(10, 10));
        this.asiContainer = asiContainer;
        this.modelSelectionCallback = modelSelectionCallback;
        setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        // Filter Panel with dynamic component wrapping
        JPanel filterPanel = new JPanel(new WrapLayout(FlowLayout.LEFT, 6, 4));
        filterPanel.setOpaque(false);

        // Extract all providers from container or models
        DefaultComboBoxModel<AbstractAiProvider> comboModel = new DefaultComboBoxModel<>();
        comboModel.addElement(null); // Represents "All AI Providers"
        List<AbstractAiProvider> allProviders = (asiContainer != null)
                ? asiContainer.getAllProviders()
                : models.stream().map(AbstractModel::getProvider).filter(Objects::nonNull).distinct().collect(Collectors.toList());
        for (AbstractAiProvider p : allProviders) {
            comboModel.addElement(p);
        }
        providerComboBox = new JComboBox<>(comboModel);
        providerComboBox.setRenderer(new AiProviderRenderer());
        providerComboBox.setSelectedItem(null);
        providerComboBox.addActionListener(e -> {
            applyFilter();
            updateAddNewModelsButton();
        });

        filterField = new JTextField(14);
        filterField.getDocument().addDocumentListener(new AnyChangeDocumentListener(this::applyFilter));

        textToggle = new JToggleButton(ResponseModality.TEXT.getDisplayName(), IconUtils.getModalityIcon(ResponseModality.TEXT, 16));
        imageToggle = new JToggleButton(ResponseModality.IMAGE.getDisplayName(), IconUtils.getModalityIcon(ResponseModality.IMAGE, 16));
        audioToggle = new JToggleButton(ResponseModality.AUDIO.getDisplayName(), IconUtils.getModalityIcon(ResponseModality.AUDIO, 16));
        videoToggle = new JToggleButton(ResponseModality.VIDEO.getDisplayName(), IconUtils.getModalityIcon(ResponseModality.VIDEO, 16));

        ActionListener toggleListener = e -> applyFilter();
        textToggle.addActionListener(toggleListener);
        imageToggle.addActionListener(toggleListener);
        audioToggle.addActionListener(toggleListener);
        videoToggle.addActionListener(toggleListener);

        effectivelyEnabledCheckbox = new JCheckBox("Effectively Enabled Only");
        effectivelyEnabledCheckbox.setSelected(false);
        effectivelyEnabledCheckbox.addActionListener(e -> applyFilter());

        refreshButton = new JButton("Refresh", new RestartIcon(16));
        refreshButton.setToolTipText("Reload model lists from providers' APIs");
        refreshButton.addActionListener(e -> refreshModelsFromProviders());

        addNewModelsButton = new JButton("Add New Models", new AddIcon(16));
        addNewModelsButton.setToolTipText("Add all unregistered API models to local database");
        addNewModelsButton.setVisible(false);
        addNewModelsButton.addActionListener(e -> addAllNewModels());

        removeSelectedButton = new JButton("Remove Selected", new DeleteIcon(16));
        removeSelectedButton.setToolTipText("Remove selected models from local storage");
        removeSelectedButton.setEnabled(false);
        removeSelectedButton.addActionListener(e -> removeSelectedModels());

        resetSelectedButton = new JButton("Reset Selected", new RestartIcon(16));
        resetSelectedButton.setToolTipText("Reset selected models with discrepancies back to API endpoint specifications");
        resetSelectedButton.setEnabled(false);
        resetSelectedButton.addActionListener(e -> resetSelectedModels());

        providerLabel = new JLabel("Provider:");
        filterPanel.add(providerLabel);
        filterPanel.add(providerComboBox);
        filterPanel.add(new JLabel("Search:"));
        filterPanel.add(filterField);
        filterPanel.add(textToggle);
        filterPanel.add(imageToggle);
        filterPanel.add(audioToggle);
        filterPanel.add(videoToggle);
        filterPanel.add(effectivelyEnabledCheckbox);
        filterPanel.add(refreshButton);
        filterPanel.add(addNewModelsButton);
        filterPanel.add(removeSelectedButton);
        filterPanel.add(resetSelectedButton);

        add(filterPanel, BorderLayout.NORTH);

        // Status Bar Panel (SOUTH)
        JPanel statusBar = new JPanel(new MigLayout("insets 4 8 4 8, fillx", "[grow,fill][]", "[]"));
        statusBar.setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, new Color(200, 200, 200)));
        statusLabel = new JLabel("Showing " + models.size() + " models");
        progressBar = new JProgressBar();
        progressBar.setIndeterminate(true);
        progressBar.setVisible(false);
        statusBar.add(statusLabel);
        statusBar.add(progressBar, "w 150!");
        add(statusBar, BorderLayout.SOUTH);

        // Table
        tableModel = new AiModelTableModel(models);
        table = new JXTable(tableModel);

        table.setColumnControlVisible(true);
        table.setHorizontalScrollEnabled(true);
        table.setFillsViewportHeight(true);
        table.setRowHeight(28);
        table.setRolloverEnabled(false);
        table.putClientProperty("terminateEditOnFocusLost", Boolean.TRUE);

        // Remove legacy SwingX CellEditorRemover listeners from KeyboardFocusManager to avoid JDK 26 Applet ClassNotFoundException
        for (PropertyChangeListener l : KeyboardFocusManager.getCurrentKeyboardFocusManager().getPropertyChangeListeners("permanentFocusOwner")) {
            if (l.getClass().getName().contains("CellEditorRemover")) {
                KeyboardFocusManager.getCurrentKeyboardFocusManager().removePropertyChangeListener("permanentFocusOwner", l);
            }
        }

        // Add double-click and right-click listeners
        table.addMouseListener(new MouseAdapter() {
            /**
             * {@inheritDoc}
             * <p>
             * Selects the row on right-click without initiating cell editing.
             * </p>
             */
            @Override
            public void mousePressed(MouseEvent e) {
                if (SwingUtilities.isRightMouseButton(e)) {
                    int row = table.rowAtPoint(e.getPoint());
                    if (row >= 0 && !table.isRowSelected(row)) {
                        table.setRowSelectionInterval(row, row);
                    }
                }
            }

            /**
             * {@inheritDoc}
             * <p>
             * Detects double-click gestures to trigger the model selection
             * callback for the row under the cursor.
             * </p>
             */
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 2 && modelSelectionCallback != null) {
                    int viewRow = table.getSelectedRow();
                    if (viewRow >= 0) {
                        int modelRow = table.convertRowIndexToModel(viewRow);
                        AbstractModel model = tableModel.getModelAt(modelRow);
                        if (model != null) {
                            modelSelectionCallback.accept(model);
                        }
                    }
                }
            }
        });

        // Add SwingX Highlighter for unregistered API models (italic + faint foreground)
        table.addHighlighter(new org.jdesktop.swingx.decorator.AbstractHighlighter() {
            @Override
            protected Component doHighlight(Component renderer, org.jdesktop.swingx.decorator.ComponentAdapter adapter) {
                int modelRow = adapter.convertRowIndexToModel(adapter.row);
                AbstractModel m = tableModel.getModelAt(modelRow);
                if (m != null && !m.isRegistered()) {
                    renderer.setFont(renderer.getFont().deriveFont(Font.ITALIC));
                    Color disabledFg = UIManager.getColor("Label.disabledForeground");
                    if (disabledFg == null) {
                        disabledFg = Color.GRAY;
                    }
                    if (!adapter.isSelected()) {
                        renderer.setForeground(disabledFg);
                    }
                }
                return renderer;
            }
        });

        // Set cell renderer and editor on Column 0 (Enabled: Checkbox for registered, [+ Add] button for unregistered)
        table.getColumnModel().getColumn(0).setCellRenderer(new Column0CellRenderer());
        table.getColumnModel().getColumn(0).setCellEditor(new Column0CellEditor());

        // Set cell renderer on AI Provider column (shows provider icon and display name)
        table.getColumnModel().getColumn(1).setCellRenderer(new AiProviderRenderer());

        // Set cell renderer on Model ID column (NewIcon if unregistered, warning if discrepancy)
        table.getColumnExt("Model ID").setCellRenderer(new DefaultTableCellRenderer() {
            private final NewIcon newBadgeIcon = new NewIcon(14);

            @Override
            public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
                super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
                int modelRow = table.convertRowIndexToModel(row);
                AbstractModel m = tableModel.getModelAt(modelRow);
                if (m != null) {
                    if (!m.isRegistered()) {
                        setText(m.getModelId());
                        setIcon(newBadgeIcon);
                        setToolTipText("Discovered from API, not yet registered in local database");
                    } else {
                        setIcon(null);
                        setText(m.getModelId());
                        if (m.hasDiscrepancy()) {
                            setToolTipText("Model has customized configuration (Reset button available to restore API defaults)");
                        } else {
                            setToolTipText(null);
                        }
                    }
                }
                return this;
            }
        });

        // Set cell renderer on Modalities column
        table.getColumnExt("Modalities").setCellRenderer(new ResponseModalitiesRenderer());
        EnumSetTableCellEditor<ResponseModality> modalityEditor = new EnumSetTableCellEditor<>(
                ResponseModality.class,
                m -> IconUtils.getModalityIcon(m, 16),
                ResponseModality::getDisplayName
        );
        table.getColumnExt("Modalities").setCellEditor(modalityEditor);
        table.getColumnExt("Modalities").setComparator((o1, o2) -> {
            List<?> l1 = (o1 instanceof List<?> l) ? l : Collections.emptyList();
            List<?> l2 = (o2 instanceof List<?> l) ? l : Collections.emptyList();
            if (l1.size() != l2.size()) {
                return Integer.compare(l1.size(), l2.size());
            }
            int max1 = l1.stream().filter(ResponseModality.class::isInstance).mapToInt(m -> ((ResponseModality) m).ordinal()).max().orElse(-1);
            int max2 = l2.stream().filter(ResponseModality.class::isInstance).mapToInt(m -> ((ResponseModality) m).ordinal()).max().orElse(-1);
            return Integer.compare(max1, max2);
        });

        // Set cell renderer and editor on Actions column
        table.getColumnExt("Actions").setCellRenderer(new ModelActionsCellRenderer());
        table.getColumnExt("Actions").setCellEditor(new ModelActionsCellEditor());

        // Update batch buttons on selection change
        table.getSelectionModel().addListSelectionListener(e -> updateSelectionActionButtons());

        // Set preferred column widths
        table.getColumnModel().getColumn(0).setPreferredWidth(60);  // Enabled
        table.getColumnModel().getColumn(1).setPreferredWidth(140); // AI Provider
        table.getColumnModel().getColumn(2).setPreferredWidth(160); // Model ID
        table.getColumnModel().getColumn(3).setPreferredWidth(150); // Display Name
        table.getColumnModel().getColumn(4).setPreferredWidth(80);  // Version
        table.getColumnModel().getColumn(5).setPreferredWidth(200); // Description
        table.getColumnModel().getColumn(6).setPreferredWidth(120); // Modalities
        table.getColumnModel().getColumn(7).setPreferredWidth(160); // Supported Actions
        table.getColumnModel().getColumn(8).setPreferredWidth(100); // Input Tokens
        table.getColumnModel().getColumn(9).setPreferredWidth(100); // Output Tokens
        table.getColumnModel().getColumn(13).setPreferredWidth(160); // Actions

        // Hide columns by default
        table.getColumnExt("Temperature").setVisible(false);
        table.getColumnExt("Top P").setVisible(false);
        table.getColumnExt("Top K").setVisible(false);

        JScrollPane scrollPane = new JScrollPane(table);
        add(scrollPane, BorderLayout.CENTER);

        updateAddNewModelsButton();
    }

    /**
     * Locks and targets this panel to a specific provider (used when embedding in AiProviderPanel).
     *
     * @param provider The target provider to lock to.
     */
    public void setTargetProvider(AbstractAiProvider provider) {
        this.targetProvider = provider;
        if (provider != null) {
            providerComboBox.setSelectedItem(provider);
            providerComboBox.setEnabled(false);
            providerComboBox.setVisible(false);
            if (providerLabel != null) {
                providerLabel.setVisible(false);
            }
            table.getColumnExt("AI Provider").setVisible(false);
            tableModel.setModels(provider.getAllDisplayModels());
            applyFilter();
            updateAddNewModelsButton();
        }
    }

    /**
     * Updates the text and visibility of the "Add X New Models" button based on unregistered API models.
     */
    private void updateAddNewModelsButton() {
        if (addNewModelsButton == null) {
            return;
        }
        int count = getUnregisteredApiModelsCount();
        if (count > 0) {
            addNewModelsButton.setText("Add " + count + " New Models");
            addNewModelsButton.setEnabled(true);
            addNewModelsButton.setVisible(true);
        } else {
            addNewModelsButton.setText("No New Models");
            addNewModelsButton.setEnabled(false);
            addNewModelsButton.setVisible(false);
        }
    }

    /**
     * Calculates the count of models discovered from API that are not yet persisted locally.
     *
     * @return The count of unregistered API models.
     */
    private int getUnregisteredApiModelsCount() {
        AbstractAiProvider selected = targetProvider != null ? targetProvider : (AbstractAiProvider) providerComboBox.getSelectedItem();
        if (selected != null) {
            Set<String> localIds = selected.getModels().stream()
                    .map(AbstractModel::getModelId)
                    .collect(Collectors.toSet());
            return (int) selected.getCachedApiModels().stream()
                    .filter(m -> !localIds.contains(m.getModelId()))
                    .count();
        }
        if (asiContainer != null) {
            return (int) asiContainer.getAllProviders().stream()
                    .mapToInt(p -> {
                        Set<String> localIds = p.getModels().stream().map(AbstractModel::getModelId).collect(Collectors.toSet());
                        return (int) p.getCachedApiModels().stream().filter(m -> !localIds.contains(m.getModelId())).count();
                    })
                    .sum();
        }
        return 0;
    }

    /**
     * Adds all newly discovered API models to the local database in a single batch.
     */
    private void addAllNewModels() {
        AbstractAiProvider selected = targetProvider != null ? targetProvider : (AbstractAiProvider) providerComboBox.getSelectedItem();
        List<AbstractAiProvider> providers = selected != null 
                ? List.of(selected) 
                : (asiContainer != null ? asiContainer.getAllProviders() : Collections.emptyList());

        int added = 0;
        for (AbstractAiProvider p : providers) {
            Set<String> localIds = p.getModels().stream()
                    .map(AbstractModel::getModelId)
                    .collect(Collectors.toSet());
            List<AbstractModel> toAdd = p.getCachedApiModels().stream()
                    .filter(m -> !localIds.contains(m.getModelId()))
                    .collect(Collectors.toList());
            for (AbstractModel m : toAdd) {
                try {
                    p.addModel(m);
                    added++;
                } catch (Exception e) {
                    log.error("Failed to add model: {}", m.getModelId(), e);
                }
            }
        }
        refreshTableData();
        statusLabel.setText("Added " + added + " new model(s) to local storage.");
    }

    /**
     * Refreshes the table view with current models from providers.
     */
    private void refreshTableData() {
        AbstractAiProvider selected = targetProvider != null ? targetProvider : (AbstractAiProvider) providerComboBox.getSelectedItem();
        List<AbstractModel> displayModels;
        if (selected != null) {
            displayModels = selected.getAllDisplayModels();
        } else if (asiContainer != null) {
            displayModels = asiContainer.getAllProviders().stream()
                    .flatMap(p -> p.getAllDisplayModels().stream())
                    .collect(Collectors.toList());
        } else {
            displayModels = Collections.emptyList();
        }
        tableModel.setModels(displayModels);
        applyFilter();
        updateAddNewModelsButton();
        updateSelectionActionButtons();
    }

    /**
     * Updates the enabled state of the 'Remove Selected' and 'Reset Selected' buttons based on active table selection.
     */
    private void updateSelectionActionButtons() {
        int[] selectedRows = table.getSelectedRows();
        if (selectedRows.length == 0) {
            removeSelectedButton.setEnabled(false);
            resetSelectedButton.setEnabled(false);
            return;
        }

        boolean hasRegistered = false;
        boolean hasDiscrepancy = false;
        for (int row : selectedRows) {
            int modelRow = table.convertRowIndexToModel(row);
            AbstractModel m = tableModel.getModelAt(modelRow);
            if (m != null && m.isRegistered()) {
                hasRegistered = true;
                if (m.hasDiscrepancy()) {
                    hasDiscrepancy = true;
                }
            }
        }
        removeSelectedButton.setEnabled(hasRegistered);
        resetSelectedButton.setEnabled(hasDiscrepancy);
    }

    /**
     * Removes all selected registered models from local storage in a single batch.
     */
    private void removeSelectedModels() {
        int[] selectedRows = table.getSelectedRows();
        List<AbstractModel> toRemove = new ArrayList<>();
        for (int row : selectedRows) {
            int modelRow = table.convertRowIndexToModel(row);
            AbstractModel m = tableModel.getModelAt(modelRow);
            if (m != null && m.isRegistered()) {
                toRemove.add(m);
            }
        }
        if (toRemove.isEmpty()) {
            return;
        }

        int confirm = JOptionPane.showConfirmDialog(this,
                "Are you sure you want to remove " + toRemove.size() + " selected model(s) from local storage?",
                "Remove Selected Models", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
        if (confirm == JOptionPane.YES_OPTION) {
            int removed = 0;
            for (AbstractModel m : toRemove) {
                try {
                    m.remove();
                    removed++;
                } catch (IOException ex) {
                    log.error("Failed to remove model {}", m.getModelId(), ex);
                }
            }
            refreshTableData();
            statusLabel.setText("Removed " + removed + " model(s) from local storage.");
        }
    }

    /**
     * Resets all selected registered models with discrepancies back to API specifications.
     */
    private void resetSelectedModels() {
        int[] selectedRows = table.getSelectedRows();
        List<AbstractModel> toReset = new ArrayList<>();
        for (int row : selectedRows) {
            int modelRow = table.convertRowIndexToModel(row);
            AbstractModel m = tableModel.getModelAt(modelRow);
            if (m != null && m.isRegistered() && m.hasDiscrepancy()) {
                toReset.add(m);
            }
        }
        if (toReset.isEmpty()) {
            JOptionPane.showMessageDialog(this, "None of the selected models have configuration discrepancies.", "No Discrepancies", JOptionPane.INFORMATION_MESSAGE);
            return;
        }

        int confirm = JOptionPane.showConfirmDialog(this,
                "Reset " + toReset.size() + " selected model(s) back to API endpoint specifications?",
                "Reset Selected Models", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
        if (confirm == JOptionPane.YES_OPTION) {
            int reset = 0;
            for (AbstractModel m : toReset) {
                try {
                    m.resetFromApi();
                    reset++;
                } catch (IOException ex) {
                    log.error("Failed to reset model {}", m.getModelId(), ex);
                }
            }
            refreshTableData();
            statusLabel.setText("Reset " + reset + " model(s) to API specifications.");
        }
    }

    /**
     * Evaluates all active filters (provider, search query regex, response modalities, and enabled status)
     * and updates the table row filter accordingly.
     */
    private void applyFilter() {
        AbstractAiProvider selectedProvider = targetProvider != null ? targetProvider : (AbstractAiProvider) providerComboBox.getSelectedItem();
        String queryText = filterField.getText().trim();
        boolean effectivelyEnabledOnly = effectivelyEnabledCheckbox.isSelected();

        Set<ResponseModality> selectedModalities = new HashSet<>();
        if (textToggle.isSelected()) selectedModalities.add(ResponseModality.TEXT);
        if (imageToggle.isSelected()) selectedModalities.add(ResponseModality.IMAGE);
        if (audioToggle.isSelected()) selectedModalities.add(ResponseModality.AUDIO);
        if (videoToggle.isSelected()) selectedModalities.add(ResponseModality.VIDEO);

        Pattern pattern = null;
        if (!queryText.isEmpty()) {
            try {
                pattern = Pattern.compile(queryText, Pattern.CASE_INSENSITIVE);
            } catch (PatternSyntaxException e) {
                pattern = Pattern.compile(Pattern.quote(queryText), Pattern.CASE_INSENSITIVE);
            }
        }

        final Pattern finalPattern = pattern;

        table.setRowFilter(new RowFilter<AiModelTableModel, Integer>() {
            @Override
            public boolean include(Entry<? extends AiModelTableModel, ? extends Integer> entry) {
                int modelRow = entry.getIdentifier();
                AbstractModel m = tableModel.getModelAt(modelRow);
                if (m == null) return false;

                // 1. Provider Filter
                if (selectedProvider != null && m.getProvider() != null && !selectedProvider.getUuid().equals(m.getProvider().getUuid())) {
                    return false;
                }

                // 2. Effectively Enabled Filter
                if (effectivelyEnabledOnly && m.getProvider() != null && !m.getProvider().isEffectivelyEnabled()) {
                    return false;
                }

                // 3. Modalities & Query Pattern Filter (delegates directly to m.matches)
                return m.matches(finalPattern, selectedModalities);
            }
        });
    }

    /**
     * Refreshes models asynchronously from providers using a SwingTask, updating the table progressively.
     */
    private void refreshModelsFromProviders() {
        AbstractAiProvider singleProvider = targetProvider != null ? targetProvider : (AbstractAiProvider) providerComboBox.getSelectedItem();
        AbstractAsiContainer targetContainer = asiContainer != null ? asiContainer : (singleProvider != null ? singleProvider.getAsiContainer() : null);

        if (targetContainer == null && singleProvider == null) {
            return;
        }

        refreshButton.setEnabled(false);
        progressBar.setVisible(true);
        statusLabel.setText("Refreshing models from API...");

        new SwingTask<List<AbstractModel>>(this, targetContainer, "Refreshing Models", () -> {
            List<AbstractAiProvider> targetProviders = singleProvider != null 
                    ? List.of(singleProvider)
                    : targetContainer.getAllProviders().stream().filter(AbstractAiProvider::isEnabled).collect(Collectors.toList());

            List<AbstractModel> accumulatedModels = new ArrayList<>();
            for (AbstractAiProvider provider : targetProviders) {
                try {
                    SwingUtilities.invokeLater(() -> statusLabel.setText("Fetching models from " + provider.getDisplayName() + "..."));
                    List<? extends AbstractModel> refreshed = provider.refreshCachedApiModels();
                    if (refreshed != null) {
                        accumulatedModels.addAll(provider.getAllDisplayModels());
                    }
                } catch (Exception ex) {
                    log.error("Failed to refresh models for provider {}", provider.getUuid(), ex);
                }
            }
            return accumulatedModels;
        }, accumulatedModels -> {
            tableModel.setModels(accumulatedModels);
            applyFilter();
            updateAddNewModelsButton();
            refreshButton.setEnabled(true);
            progressBar.setVisible(false);
            statusLabel.setText("Refreshed API models (" + accumulatedModels.size() + " total models).");
        }, error -> {
            log.error("Error refreshing models", error);
            refreshButton.setEnabled(true);
            progressBar.setVisible(false);
            statusLabel.setText("Error refreshing models: " + error.getMessage());
        }).start();
    }

    /**
     * Stateless cell renderer for Column 0.
     */
    private class Column0CellRenderer implements TableCellRenderer {

        private final JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER, 0, 2));
        private final JCheckBox checkBox = new JCheckBox();
        private final JButton addBtn = new JButton("Add", new AddIcon(12));

        public Column0CellRenderer() {
            panel.setOpaque(true);
            checkBox.setOpaque(false);
            addBtn.setFont(addBtn.getFont().deriveFont(11f));
            addBtn.setMargin(new java.awt.Insets(1, 4, 1, 4));
        }

        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
            int modelRow = table.convertRowIndexToModel(row);
            AbstractModel model = tableModel.getModelAt(modelRow);
            panel.removeAll();
            panel.setBackground(isSelected ? table.getSelectionBackground() : table.getBackground());
            if (model != null) {
                if (model.isRegistered()) {
                    checkBox.setSelected(model.isEnabled());
                    panel.add(checkBox);
                } else {
                    panel.add(addBtn);
                }
            }
            return panel;
        }
    }

    /**
     * Active cell editor for Column 0.
     */
    private class Column0CellEditor extends AbstractCellEditor implements TableCellEditor {

        private final JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER, 0, 2));
        private final JCheckBox checkBox = new JCheckBox();
        private final JButton addBtn = new JButton("Add", new AddIcon(12));
        private AbstractModel editingModel;

        public Column0CellEditor() {
            panel.setOpaque(true);
            checkBox.setOpaque(false);
            addBtn.setFont(addBtn.getFont().deriveFont(11f));
            addBtn.setMargin(new java.awt.Insets(1, 4, 1, 4));

            addBtn.addActionListener(e -> {
                if (editingModel != null && editingModel.getProvider() != null) {
                    try {
                        editingModel.getProvider().addModel(editingModel);
                        fireEditingStopped();
                        refreshTableData();
                    } catch (IOException ex) {
                        log.error("Failed to add model {}", editingModel.getModelId(), ex);
                        JOptionPane.showMessageDialog(AiModelsPanel.this, "Failed to add model: " + ex.getMessage());
                    }
                }
            });

            checkBox.addActionListener(e -> {
                if (editingModel != null && editingModel.isRegistered()) {
                    editingModel.setEnabled(checkBox.isSelected());
                    try {
                        editingModel.persist();
                    } catch (IOException ex) {
                        log.error("Failed to persist model enabled state {}", editingModel.getModelId(), ex);
                    }
                    fireEditingStopped();
                }
            });
        }

        @Override
        public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
            int modelRow = table.convertRowIndexToModel(row);
            this.editingModel = tableModel.getModelAt(modelRow);
            panel.removeAll();
            panel.setBackground(table.getSelectionBackground());
            if (editingModel != null) {
                if (editingModel.isRegistered()) {
                    checkBox.setSelected(editingModel.isEnabled());
                    panel.add(checkBox);
                } else {
                    panel.add(addBtn);
                }
            }
            return panel;
        }

        @Override
        public Object getCellEditorValue() {
            return editingModel;
        }
    }

    /**
     * Stateless cell renderer providing Remove and Reset buttons for table rows.
     */
    private class ModelActionsCellRenderer implements TableCellRenderer {

        private final JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER, 4, 2));
        private final JButton removeBtn = new JButton("Remove", new DeleteIcon(12));
        private final JButton resetBtn = new JButton("Reset", new RestartIcon(12));

        public ModelActionsCellRenderer() {
            panel.setOpaque(true);
            removeBtn.setFont(removeBtn.getFont().deriveFont(11f));
            removeBtn.setMargin(new java.awt.Insets(1, 4, 1, 4));
            resetBtn.setFont(resetBtn.getFont().deriveFont(11f));
            resetBtn.setMargin(new java.awt.Insets(1, 4, 1, 4));
        }

        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
            int modelRow = table.convertRowIndexToModel(row);
            AbstractModel model = tableModel.getModelAt(modelRow);
            panel.removeAll();
            panel.setBackground(isSelected ? table.getSelectionBackground() : table.getBackground());

            if (model != null && model.isRegistered()) {
                panel.add(removeBtn);
                if (model.hasDiscrepancy()) {
                    panel.add(resetBtn);
                }
            }
            return panel;
        }
    }

    /**
     * Active cell editor providing Remove and Reset buttons for table rows.
     */
    private class ModelActionsCellEditor extends AbstractCellEditor implements TableCellEditor {

        private final JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER, 4, 2));
        private final JButton removeBtn = new JButton("Remove", new DeleteIcon(12));
        private final JButton resetBtn = new JButton("Reset", new RestartIcon(12));
        private AbstractModel editingModel;

        public ModelActionsCellEditor() {
            panel.setOpaque(true);
            removeBtn.setFont(removeBtn.getFont().deriveFont(11f));
            removeBtn.setMargin(new java.awt.Insets(1, 4, 1, 4));
            resetBtn.setFont(resetBtn.getFont().deriveFont(11f));
            resetBtn.setMargin(new java.awt.Insets(1, 4, 1, 4));

            removeBtn.addActionListener(e -> {
                if (editingModel != null) {
                    int confirm = JOptionPane.showConfirmDialog(AiModelsPanel.this,
                            "Are you sure you want to remove model '" + editingModel.getModelId() + "' from local storage?",
                            "Remove Model", JOptionPane.YES_NO_OPTION);
                    if (confirm == JOptionPane.YES_OPTION) {
                        try {
                            editingModel.remove();
                            fireEditingStopped();
                            refreshTableData();
                        } catch (IOException ex) {
                            log.error("Failed to remove model {}", editingModel.getModelId(), ex);
                            JOptionPane.showMessageDialog(AiModelsPanel.this, "Failed to remove model: " + ex.getMessage());
                        }
                    }
                }
            });

            resetBtn.addActionListener(e -> {
                if (editingModel != null) {
                    try {
                        editingModel.resetFromApi();
                        fireEditingStopped();
                        refreshTableData();
                    } catch (IOException ex) {
                        log.error("Failed to reset model {}", editingModel.getModelId(), ex);
                        JOptionPane.showMessageDialog(AiModelsPanel.this, "Failed to reset model: " + ex.getMessage());
                    }
                }
            });
        }

        @Override
        public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
            int modelRow = table.convertRowIndexToModel(row);
            this.editingModel = tableModel.getModelAt(modelRow);
            panel.removeAll();
            panel.setBackground(table.getSelectionBackground());

            if (editingModel != null && editingModel.isRegistered()) {
                panel.add(removeBtn);
                if (editingModel.hasDiscrepancy()) {
                    panel.add(resetBtn);
                }
            }
            return panel;
        }

        @Override
        public Object getCellEditorValue() {
            return editingModel;
        }
    }
}
