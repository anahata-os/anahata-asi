/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;
import javax.swing.table.AbstractTableModel;
import lombok.NonNull;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.status.AgiStatus;

/**
 * A reusable table model for displaying active AI agi sessions.
 * This model tracks the {@link AsiContainer} and provides real-time updates
 * on session status, message count, and context usage.
 * 
 * @author gemini-3-flash-preview
 */
public class AgisTableModel extends AbstractTableModel {

    private final List<Agi> sessions = new ArrayList<>();
    private final String[] columnNames = {"Nickname", "ID", "Status", "Msgs", "Context %"};
    private final AsiContainer asiConfig;
    private final PropertyChangeListener asiListener = this::handleAsiChange;

    public static final int SESSION_COL = 0;
    public static final int ID_COL = 1;
    public static final int STATUS_COL = 2;
    public static final int MESSAGES_COL = 3;
    public static final int CONTEXT_COL = 4;

    public AgisTableModel(@NonNull AsiContainer asiConfig) {
        this.asiConfig = asiConfig;
        refresh();
        asiConfig.addPropertyChangeListener(asiListener);
    }

    @Override
    public int getRowCount() {
        return sessions.size();
    }

    @Override
    public int getColumnCount() {
        return columnNames.length;
    }

    @Override
    public String getColumnName(int column) {
        return columnNames[column];
    }

    @Override
    public Class<?> getColumnClass(int columnIndex) {
        switch (columnIndex) {
            case MESSAGES_COL:
                return Integer.class;
            case CONTEXT_COL:
                return Double.class;
            case STATUS_COL:
                return AgiStatus.class;
            default:
                return String.class;
        }
    }

    @Override
    public Object getValueAt(int rowIndex, int columnIndex) {
        if (rowIndex < 0 || rowIndex >= sessions.size()) {
            return null;
        }
        Agi agi = sessions.get(rowIndex);

        switch (columnIndex) {
            case SESSION_COL:
                return agi.getDisplayName();
            case ID_COL:
                return agi.getShortId();
            case STATUS_COL:
                return agi.getStatusManager().getCurrentStatus();
            case MESSAGES_COL:
                return agi.getContextManager().getHistory().size();
            case CONTEXT_COL:
                return agi.getContextWindowUsage();
            default:
                return null;
        }
    }

    /**
     * Refreshes the table model by synchronizing with the {@link AsiContainer}.
     */
    public final void refresh() {
        List<Agi> activeAgis = asiConfig.getActiveAgis();
        
        // Identify removed sessions
        for (int i = sessions.size() - 1; i >= 0; i--) {
            if (!activeAgis.contains(sessions.get(i))) {
                sessions.remove(i);
                fireTableRowsDeleted(i, i);
            }
        }

        // Identify added sessions
        for (int i = 0; i < activeAgis.size(); i++) {
            Agi agi = activeAgis.get(i);
            if (!sessions.contains(agi)) {
                sessions.add(i, agi);
                fireTableRowsInserted(i, i);
            }
        }

        // Identify updated rows
        if (!sessions.isEmpty()) {
            fireTableRowsUpdated(0, sessions.size() - 1);
        }
    }

    public Agi getAgiAt(int row) {
        if (row >= 0 && row < sessions.size()) {
            return sessions.get(row);
        }
        return null;
    }

    private void handleAsiChange(PropertyChangeEvent evt) {
        if ("activeAgis".equals(evt.getPropertyName())) {
            refresh();
        }
    }
    
    public void dispose() {
        asiConfig.removePropertyChangeListener(asiListener);
    }
}
