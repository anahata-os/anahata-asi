/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.provider;

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
import uno.anahata.ai.model.provider.AbstractModel;
import uno.anahata.ai.swing.internal.AnyChangeDocumentListener;

/**
 * A panel for viewing and filtering the registered AI models.
 * It uses a JXTable for advanced table features.
 * 
 * @author anahata
 */
public class ProviderRegistryViewer extends JPanel {

    private final JXTable table;
    private final ModelTableModel tableModel;
    private final JTextField filterField;
    private final Consumer<AbstractModel> modelSelectionCallback;

    /**
     * Constructs a new ProviderRegistryViewer.
     * 
     * @param models The list of models to display.
     * @param modelSelectionCallback A callback for when a model is double-clicked.
     */
    public ProviderRegistryViewer(List<AbstractModel> models, Consumer<AbstractModel> modelSelectionCallback) {
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
        tableModel = new ModelTableModel(models);
        
        table = new JXTable(tableModel) {
            
            /** {@inheritDoc} */
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
            /** {@inheritDoc} */
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
        table.getColumnModel().getColumn(0).setPreferredWidth(150); // Model ID
        table.getColumnModel().getColumn(1).setPreferredWidth(150); // Display Name
        table.getColumnModel().getColumn(2).setPreferredWidth(80);  // Version
        table.getColumnModel().getColumn(3).setPreferredWidth(250); // Description
        table.getColumnModel().getColumn(4).setPreferredWidth(200); // Supported Actions
        table.getColumnModel().getColumn(5).setPreferredWidth(100); // Input Tokens
        table.getColumnModel().getColumn(6).setPreferredWidth(100); // Output Tokens

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
