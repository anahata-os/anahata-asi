/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.provider;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import javax.swing.table.AbstractTableModel;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.ResponseModality;
import uno.anahata.asi.internal.TypeParsers;

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
@Slf4j
public class AiModelTableModel extends AbstractTableModel {

    /** The ordered set of column headers reflecting model specifications. */
    private final String[] columnNames = {
        "Enabled", "AI Provider", "Model ID", "Display Name", "Version", "Description",
        "Modalities", "Supported Actions", "Input Tokens", "Output Tokens",
        "Temperature", "Top P", "Top K", "Actions"
    };
    /** The backing list of model entities. */
    private final List<AbstractModel> models;

    /**
     * Constructs a new ModelTableModel.
     * 
     * @param models The list of models to display.
     */
    public AiModelTableModel(List<AbstractModel> models) {
        this.models = new ArrayList<>(models != null ? models : Collections.emptyList());
    }

    /**
     * Updates the underlying model list and notifies listeners of the change.
     * 
     * @param newModels The new list of models.
     */
    public void setModels(List<AbstractModel> newModels) {
        this.models.clear();
        if (newModels != null) {
            this.models.addAll(newModels);
        }
        fireTableDataChanged();
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
     */
    @Override
    public Class<?> getColumnClass(int columnIndex) {
        return switch (columnIndex) {
            case 0 -> Boolean.class;
            case 6 -> List.class;
            case 8, 9, 12 -> Integer.class;
            case 10, 11 -> Float.class;
            default -> Object.class;
        };
    }

    /**
     * {@inheritDoc}
     * <p>Allows editing Column 0, String metadata, numeric parameters, and Column 13 for registered models.</p>
     */
    @Override
    public boolean isCellEditable(int rowIndex, int columnIndex) {
        AbstractModel model = getModelAt(rowIndex);
        if (model == null) {
            return false;
        }
        if (columnIndex == 0) {
            return true;
        }
        if (columnIndex == 13) {
            return model.isRegistered();
        }
        if (!model.isRegistered()) {
            return false;
        }
        return switch (columnIndex) {
            case 3, 4, 5, 6, 8, 9, 10, 11, 12 -> true;
            default -> false;
        };
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
        AbstractModel model = getModelAt(rowIndex);
        if (model == null || !model.isRegistered()) {
            return;
        }
        try {
            switch (columnIndex) {
                case 0 -> {
                    if (aValue instanceof Boolean b) {
                        model.setEnabled(b);
                    }
                }
                case 3 -> model.setDisplayName(aValue != null ? aValue.toString().trim() : "");
                case 4 -> model.setVersion(aValue != null ? aValue.toString().trim() : "");
                case 5 -> model.setDescription(aValue != null ? aValue.toString().trim() : "");
                case 6 -> model.setSupportedResponseModalities(TypeParsers.parseEnumList(aValue, ResponseModality.class));
                case 8 -> model.setMaxInputTokens(TypeParsers.parseInteger(aValue));
                case 9 -> model.setMaxOutputTokens(TypeParsers.parseInteger(aValue));
                case 10 -> model.setDefaultTemperature(TypeParsers.parseFloat(aValue));
                case 11 -> model.setDefaultTopP(TypeParsers.parseFloat(aValue));
                case 12 -> model.setDefaultTopK(TypeParsers.parseInteger(aValue));
            }
            model.persist();
            fireTableRowsUpdated(rowIndex, rowIndex);
        } catch (IOException ex) {
            log.error("Failed to persist model changes for {}", model.getModelId(), ex);
        }
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
            case 0: return model;
            case 1: return model.getProvider();
            case 2: return model.getModelId();
            case 3: return model.getDisplayName();
            case 4: return model.getVersion();
            case 5: return model.getDescription();
            case 6: return model.getSupportedResponseModalities();
            case 7: return String.join(", ", model.getSupportedActions());
            case 8: return model.getMaxInputTokens();
            case 9: return model.getMaxOutputTokens();
            case 10: return model.getDefaultTemperature();
            case 11: return model.getDefaultTopP();
            case 12: return model.getDefaultTopK();
            case 13: return model;
            default: return null;
        }
    }
}
