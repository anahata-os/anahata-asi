/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.provider;

import java.awt.BorderLayout;
import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.List;
import java.util.function.Consumer;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.RowFilter;
import org.jdesktop.swingx.JXTable;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.swing.internal.AnyChangeDocumentListener;

/**
 * A high-fidelity visual registry for exploring and selecting registered AI models.
 * <p>
 * This panel utilizes a {@link org.jdesktop.swingx.JXTable} to provide advanced 
 * discovery features such as real-time regex filtering, dynamic column 
 * visibility, and high-performance horizontal scrolling. It acts as the primary 
 * UI for model disambiguation and selection.
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
    /** Reactive callback for notifying the system of a user's model selection. */
    private final Consumer<AbstractModel> modelSelectionCallback;

    /**
     * Constructs a new ProviderRegistryViewer.
     * 
     * @param models The list of models to display.
     * @param modelSelectionCallback A callback for when a model is double-clicked.
     */
    public AiProviderRegistryViewer(List<AbstractModel> models, Consumer<AbstractModel> modelSelectionCallback) {
        super(new BorderLayout(10, 10));
        this.modelSelectionCallback = modelSelectionCallback;
        setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        // Filter Panel
        JPanel filterPanel = new JPanel(new BorderLayout(5, 5));
        filterField = new JTextField();
        filterPanel.add(new JLabel("Filter:"), BorderLayout.WEST);
        filterPanel.add(filterField, BorderLayout.CENTER);
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
        
        // Set preferred column widths
        table.getColumnModel().getColumn(0).setPreferredWidth(120); // AI Provider
        table.getColumnModel().getColumn(1).setPreferredWidth(150); // Model ID
        table.getColumnModel().getColumn(2).setPreferredWidth(150); // Display Name
        table.getColumnModel().getColumn(3).setPreferredWidth(80);  // Version
        table.getColumnModel().getColumn(4).setPreferredWidth(250); // Description
        table.getColumnModel().getColumn(5).setPreferredWidth(200); // Supported Actions
        table.getColumnModel().getColumn(6).setPreferredWidth(100); // Input Tokens
        table.getColumnModel().getColumn(7).setPreferredWidth(100); // Output Tokens

        // Hide columns by default (user can show them via column control)
        // Use column names as identifiers to avoid index shifting issues
        table.getColumnExt("Model ID").setVisible(false);
        table.getColumnExt("Temperature").setVisible(false);
        table.getColumnExt("Top P").setVisible(false);
        table.getColumnExt("Top K").setVisible(false);

        JScrollPane scrollPane = new JScrollPane(table);
        add(scrollPane, BorderLayout.CENTER);

        // Filter logic
        filterField.getDocument().addDocumentListener(new AnyChangeDocumentListener(this::applyFilter));
    }

    /**
     * Applies the filter from the filter field to the table.
     */
    private void applyFilter() {
        String text = filterField.getText();
        table.setRowFilter(text.trim().isEmpty() ? null : RowFilter.regexFilter("(?i)" + text));
    }
}
