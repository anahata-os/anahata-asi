/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.provider;

import java.util.List;
import javax.swing.table.AbstractTableModel;
import uno.anahata.asi.agi.provider.AbstractModel;

/**
 * A specialized {@link javax.swing.table.TableModel} for rendering the technical 
 * specification and capabilities of AI models.
 * <p>
 * This model translates {@link AbstractModel} entities into a tabular format, 
 * exposing critical parameters like token limits, supported actions, and 
 * sampling defaults (temperature, Top-P, etc.).
 * </p>
 * 
 * @author anahata
 */
public class AiModelTableModel extends AbstractTableModel {

    /** The ordered set of column headers reflecting model specifications. */
    private final String[] columnNames = {
        "AI Provider", "Model ID", "Display Name", "Version", "Description",
        "Supported Actions", "Input Tokens", "Output Tokens",
        "Temperature", "Top P", "Top K"
    };
    /** The backing list of model entities. */
    private final List<AbstractModel> models;

    /**
     * Constructs a new ModelTableModel.
     * 
     * @param models The list of models to display.
     */
    public AiModelTableModel(List<AbstractModel> models) {
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

    /** 
     * {@inheritDoc} 
     * <p>Returns the total number of registered models in the registry.</p> 
     */
    @Override
    public int getRowCount() {
        return models.size();
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the number of technical parameters exposed by the model.</p> 
     */
    @Override
    public int getColumnCount() {
        return columnNames.length;
    }

    /** 
     * {@inheritDoc} 
     * <p>Provides the descriptive header for the technical parameter column.</p> 
     */
    @Override
    public String getColumnName(int column) {
        return columnNames[column];
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Extracts and formats specific technical attributes from the {@link AbstractModel} 
     * entity based on the column index.
     * </p> 
     */
    @Override
    public Object getValueAt(int rowIndex, int columnIndex) {
        AbstractModel model = models.get(rowIndex);
        switch (columnIndex) {
            case 0: return model.getProviderId();
            case 1: return model.getModelId();
            case 2: return model.getDisplayName();
            case 3: return model.getVersion();
            case 4: return model.getDescription();
            case 5: return String.join(", ", model.getSupportedActions());
            case 6: return model.getMaxInputTokens();
            case 7: return model.getMaxOutputTokens();
            case 8: return model.getDefaultTemperature();
            case 9: return model.getDefaultTopP();
            case 10: return model.getDefaultTopK();
            default: return null;
        }
    }
}
