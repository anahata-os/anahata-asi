/*
 * Copyright 2025 Anahata.
 *
 * Licensed under the Anahata Software License (ASL) V2.0;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://github.com/pablo-anahata/anahata-ai-parent/blob/main/LICENSE
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Força Barça!
 */
package uno.anahata.ai.swing.provider;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableRowSorter;
import uno.anahata.ai.AiConfig;

import uno.anahata.ai.model.provider.AbstractAiProvider;
import uno.anahata.ai.model.provider.AiProviderRegistry;
import uno.anahata.ai.model.provider.AbstractModel;

public class ProviderRegistryViewer extends JPanel {

    private final JTable table;
    private final ModelTableModel tableModel;
    private final JTextField filterField;
    private final TableRowSorter<ModelTableModel> sorter;

    public ProviderRegistryViewer(List<AbstractModel> models) {
        super(new BorderLayout(10, 10));
        setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        // Filter Panel
        JPanel filterPanel = new JPanel(new BorderLayout(5, 5));
        filterField = new JTextField();
        filterPanel.add(new JLabel("Filter:"), BorderLayout.WEST);
        filterPanel.add(filterField, BorderLayout.CENTER);
        add(filterPanel, BorderLayout.NORTH);

        // Table
        tableModel = new ModelTableModel(models);
        sorter = new TableRowSorter<>(tableModel);
        
        table = new JTable(tableModel) {
            
            @Override
            public JToolTip createToolTip() {
                // Use the HTML-based custom tooltip
                return new HtmlWrappingToolTip();
            }
            
            @Override
            public String getToolTipText(MouseEvent e) {
                Point p = e.getPoint();
                int viewRow = rowAtPoint(p);
                if (viewRow >= 0) {
                    int modelRow = convertRowIndexToModel(viewRow);
                    AbstractModel model = tableModel.getModelAt(modelRow);
                    if (model != null) {
                        // Return the HTML version for the JTextPane in the custom tooltip
                        return model.getRawDescription();
                    }
                }
                return super.getToolTipText(e);
            }
        };
        
        table.setRowSorter(sorter);
        table.setFillsViewportHeight(true);
        
        // Set preferred column widths
        table.getColumnModel().getColumn(0).setPreferredWidth(180); // Display Name
        table.getColumnModel().getColumn(1).setPreferredWidth(180); // Model ID
        table.getColumnModel().getColumn(2).setPreferredWidth(80);  // Generate
        table.getColumnModel().getColumn(3).setPreferredWidth(80);  // Functions
        table.getColumnModel().getColumn(4).setPreferredWidth(80);  // Embed
        table.getColumnModel().getColumn(5).setPreferredWidth(80);  // Batch Embed
        table.getColumnModel().getColumn(6).setPreferredWidth(80);  // Cache
        table.getColumnModel().getColumn(7).setPreferredWidth(100); // Input Tokens
        table.getColumnModel().getColumn(8).setPreferredWidth(100); // Output Tokens

        JScrollPane scrollPane = new JScrollPane(table);
        add(scrollPane, BorderLayout.CENTER);

        // Filter logic
        filterField.getDocument().addDocumentListener(new javax.swing.event.DocumentListener() {
            public void insertUpdate(javax.swing.event.DocumentEvent e) { applyFilter(); }
            public void removeUpdate(javax.swing.event.DocumentEvent e) { applyFilter(); }
            public void changedUpdate(javax.swing.event.DocumentEvent e) { applyFilter(); }
        });
    }

    private void applyFilter() {
        String text = filterField.getText();
        sorter.setRowFilter(text.trim().isEmpty() ? null : RowFilter.regexFilter("(?i)" + text));
    }

    private static class ModelTableModel extends AbstractTableModel {
        private final String[] columnNames = {
            "Display Name", "Model ID", "Generate", "Functions", "Embed", "Batch Embed", "Cache",
            "Input Tokens", "Output Tokens"
        };
        private final List<AbstractModel> models;

        public ModelTableModel(List<AbstractModel> models) {
            this.models = models;
        }
        
        public AbstractModel getModelAt(int rowIndex) {
            if (rowIndex >= 0 && rowIndex < models.size()) {
                return models.get(rowIndex);
            }
            return null;
        }

        @Override public int getRowCount() { return models.size(); }
        @Override public int getColumnCount() { return columnNames.length; }
        @Override public String getColumnName(int column) { return columnNames[column]; }
        @Override public Class<?> getColumnClass(int columnIndex) {
            return (columnIndex >= 2 && columnIndex <= 6) ? Boolean.class : super.getColumnClass(columnIndex);
        }

        @Override
        public Object getValueAt(int rowIndex, int columnIndex) {
            AbstractModel model = models.get(rowIndex);
            switch (columnIndex) {
                case 0: return model.getDisplayName();
                case 1: return model.getModelId();
                case 2: return model.isSupportsContentGeneration();
                case 3: return model.isSupportsFunctionCalling();
                case 4: return model.isSupportsEmbeddings();
                case 5: return model.isSupportsBatchEmbeddings();
                case 6: return model.isSupportsCachedContent();
                case 7: return model.getMaxInputTokens();
                case 8: return model.getMaxOutputTokens();
                default: return null;
            }
        }
    }

    /** Test main method to display the panel in a JFrame */
    public static void main(String[] args) {
        // Configure SLF4J Simple Logger to show DEBUG messages
        System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "debug");
        
        AiConfig config = new AiConfig("test");
        AiProviderRegistry registry = new AiProviderRegistry();
        /*
        AbstractAiProvider geminiProvider = new GeminiAiProvider(config);
        registry.registerProvider(geminiProvider);

        // Use the caching getModels() method
        List<AbstractAiModel> allModels = new ArrayList<>();
        registry.getProviders().forEach(provider -> allModels.addAll(provider.getModels()));

        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame("Anahata AI Provider Registry Viewer");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.getContentPane().add(new ProviderRegistryViewer(allModels));
            frame.setPreferredSize(new Dimension(1200, 800));
            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
        });
*/
    }
}
