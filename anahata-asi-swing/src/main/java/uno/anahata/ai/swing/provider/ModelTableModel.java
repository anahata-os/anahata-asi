/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara! */
package uno.anahata.ai.swing.provider;

import java.util.List;
import javax.swing.table.AbstractTableModel;
import uno.anahata.ai.model.provider.AbstractModel;

/**
 * A table model for displaying AI models and their capabilities.
 * 
 * @author anahata
 */
public class ModelTableModel extends AbstractTableModel {

    private final String[] columnNames = {
        "Model ID", "Display Name", "Version", "Description",
        "Supported Actions", "Input Tokens", "Output Tokens",
        "Temperature", "Top P", "Top K"
    };
    private final List<AbstractModel> models;

    /**
     * Constructs a new ModelTableModel.
     * 
     * @param models The list of models to display.
     */
    public ModelTableModel(List<AbstractModel> models) {
        this.models = models;
    }

    /**
     * Gets the model at the specified row index.
     * 
     * @param rowIndex The row index.
     * @return The AbstractModel, or null if the index is out of bounds.
     */
    public AbstractModel getModelAt(int rowIndex) {
        if (rowIndex >= 0 && rowIndex < models.size()) {
            return models.get(rowIndex);
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public int getRowCount() {
        return models.size();
    }

    /** {@inheritDoc} */
    @Override
    public int getColumnCount() {
        return columnNames.length;
    }

    /** {@inheritDoc} */
    @Override
    public String getColumnName(int column) {
        return columnNames[column];
    }

    /** {@inheritDoc} */
    @Override
    public Object getValueAt(int rowIndex, int columnIndex) {
        AbstractModel model = models.get(rowIndex);
        switch (columnIndex) {
            case 0: return model.getModelId();
            case 1: return model.getDisplayName();
            case 2: return model.getVersion();
            case 3: return model.getDescription();
            case 4: return String.join(", ", model.getSupportedActions());
            case 5: return model.getMaxInputTokens();
            case 6: return model.getMaxOutputTokens();
            case 7: return model.getDefaultTemperature();
            case 8: return model.getDefaultTopP();
            case 9: return model.getDefaultTopK();
            default: return null;
        }
    }
}
