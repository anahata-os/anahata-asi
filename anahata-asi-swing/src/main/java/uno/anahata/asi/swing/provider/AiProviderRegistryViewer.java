/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.provider;

import java.awt.BorderLayout;
import java.awt.Point;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import java.util.stream.Collectors;
import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.RowFilter;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXTable;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.ResponseModality;
import uno.anahata.asi.swing.icons.ImageModalityIcon;
import uno.anahata.asi.swing.icons.SpeakerIcon;
import uno.anahata.asi.swing.icons.TextModalityIcon;
import uno.anahata.asi.swing.icons.VideoModalityIcon;
import uno.anahata.asi.swing.internal.AnyChangeDocumentListener;

/**
 * A high-fidelity visual registry for exploring and selecting registered AI models.
 * <p>
 * This panel utilizes a {@link org.jdesktop.swingx.JXTable} and rich filter controls
 * (provider dropdown with icons, keyword/regex query field, response modality toggles,
 * and effectively-enabled state filter) to provide advanced discovery features.
 * Acts as the primary UI for model disambiguation and selection.
 * </p>
 * 
 * @author anahata
 */
public class AiProviderRegistryViewer extends JPanel {

    /** The advanced SwingX table instance for model discovery. */
    private final JXTable table;
    /** The technical data model powering the table. */
    private final AiModelTableModel tableModel;
    /** The real-time search and filter input field. */
    private final JTextField filterField;
    /** The dropdown combo box for selecting/filtering by AI provider. */
    private final JComboBox<AbstractAiProvider> providerComboBox;
    /** Toggle button for filtering by TEXT response modality. */
    private final JToggleButton textToggle;
    /** Toggle button for filtering by IMAGE response modality. */
    private final JToggleButton imageToggle;
    /** Toggle button for filtering by AUDIO response modality. */
    private final JToggleButton audioToggle;
    /** Toggle button for filtering by VIDEO response modality. */
    private final JToggleButton videoToggle;
    /** Checkbox to filter models to only effectively enabled providers. */
    private final JCheckBox effectivelyEnabledCheckbox;
    /** Reactive callback for notifying the system of a user's model selection. */
    private final Consumer<AbstractModel> modelSelectionCallback;

    /**
     * Constructs a new ProviderRegistryViewer with full search, filter, and selection capabilities.
     * 
     * @param models The list of models to display.
     * @param modelSelectionCallback A callback for when a model is double-clicked.
     */
    public AiProviderRegistryViewer(List<AbstractModel> models, Consumer<AbstractModel> modelSelectionCallback) {
        super(new BorderLayout(10, 10));
        this.modelSelectionCallback = modelSelectionCallback;
        setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        // Filter Panel
        JPanel filterPanel = new JPanel(new MigLayout("insets 0, fillx", "[][180!,grow 0][][grow,fill][][][][][]", "[]"));
        
        // Extract unique providers from models
        DefaultComboBoxModel<AbstractAiProvider> comboModel = new DefaultComboBoxModel<>();
        comboModel.addElement(null); // Represents "All AI Providers"
        Set<String> seenUuids = new HashSet<>();
        for (AbstractModel m : models) {
            if (m.getProvider() != null && seenUuids.add(m.getProvider().getUuid())) {
                comboModel.addElement(m.getProvider());
            }
        }
        providerComboBox = new JComboBox<>(comboModel);
        providerComboBox.setRenderer(new AiProviderRenderer());
        providerComboBox.setSelectedItem(null);
        providerComboBox.addActionListener(e -> applyFilter());

        filterField = new JTextField();
        filterField.getDocument().addDocumentListener(new AnyChangeDocumentListener(this::applyFilter));

        textToggle = new JToggleButton("TEXT", new TextModalityIcon(16));
        imageToggle = new JToggleButton("IMAGE", new ImageModalityIcon(16));
        audioToggle = new JToggleButton("AUDIO", new SpeakerIcon(16));
        videoToggle = new JToggleButton("VIDEO", new VideoModalityIcon(16));

        ActionListener toggleListener = e -> applyFilter();
        textToggle.addActionListener(toggleListener);
        imageToggle.addActionListener(toggleListener);
        audioToggle.addActionListener(toggleListener);
        videoToggle.addActionListener(toggleListener);

        effectivelyEnabledCheckbox = new JCheckBox("Effectively Enabled Only");
        effectivelyEnabledCheckbox.setSelected(false);
        effectivelyEnabledCheckbox.addActionListener(e -> applyFilter());

        filterPanel.add(new JLabel("Provider:"));
        filterPanel.add(providerComboBox);
        filterPanel.add(new JLabel("Search:"));
        filterPanel.add(filterField);
        filterPanel.add(textToggle);
        filterPanel.add(imageToggle);
        filterPanel.add(audioToggle);
        filterPanel.add(videoToggle);
        filterPanel.add(effectivelyEnabledCheckbox);

        add(filterPanel, BorderLayout.NORTH);

        // Table
        tableModel = new AiModelTableModel(models);
        
        table = new JXTable(tableModel) {
            
            /** 
             * {@inheritDoc} 
             * <p>
             * Provides the full, non-truncated model description as a tooltip 
             * when hovering over a specific row.
             * </p> 
             */
            @Override
            public String getToolTipText(MouseEvent e) {
                Point p = e.getPoint();
                int viewRow = rowAtPoint(p);
                if (viewRow >= 0) {
                    int modelRow = convertRowIndexToModel(viewRow);
                    AbstractModel model = tableModel.getModelAt(modelRow);
                    if (model != null) {
                        return model.getRawDescription();
                    }
                }
                return super.getToolTipText(e);
            }
        };
        
        table.setColumnControlVisible(true);
        table.setHorizontalScrollEnabled(true);
        table.setFillsViewportHeight(true);
        
        // Add double-click listener
        table.addMouseListener(new MouseAdapter() {
            /** 
             * {@inheritDoc} 
             * <p>
             * Detects double-click gestures to trigger the model selection callback 
             * for the row under the cursor.
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
        
        // Set cell renderer on AI Provider column (shows provider icon and display name)
        table.getColumnModel().getColumn(0).setCellRenderer(new AiProviderRenderer());
        table.getColumnExt("Modalities").setCellRenderer(new ResponseModalitiesRenderer());
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

        // Set preferred column widths
        table.getColumnModel().getColumn(0).setPreferredWidth(140); // AI Provider
        table.getColumnModel().getColumn(1).setPreferredWidth(150); // Model ID
        table.getColumnModel().getColumn(2).setPreferredWidth(150); // Display Name
        table.getColumnModel().getColumn(3).setPreferredWidth(80);  // Version
        table.getColumnModel().getColumn(4).setPreferredWidth(250); // Description
        table.getColumnModel().getColumn(5).setPreferredWidth(200); // Supported Actions
        table.getColumnModel().getColumn(6).setPreferredWidth(100); // Input Tokens
        table.getColumnModel().getColumn(7).setPreferredWidth(100); // Output Tokens

        // Hide columns by default (user can show them via column control)
        table.getColumnExt("Model ID").setVisible(false);
        table.getColumnExt("Temperature").setVisible(false);
        table.getColumnExt("Top P").setVisible(false);
        table.getColumnExt("Top K").setVisible(false);

        JScrollPane scrollPane = new JScrollPane(table);
        add(scrollPane, BorderLayout.CENTER);
    }

    /**
     * Evaluates all active filters (provider, search query regex, response modalities, and enabled status)
     * and updates the table row filter accordingly.
     */
    private void applyFilter() {
        AbstractAiProvider selectedProvider = (AbstractAiProvider) providerComboBox.getSelectedItem();
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
}
