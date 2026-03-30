/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.swing.table.AbstractTableModel;
import lombok.NonNull;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.status.AgiStatus;

/**
 * A reusable table model for displaying active AI agi sessions.
 * This model tracks the {@link AbstractAsiContainer} and provides real-time updates
 * on session status, message count, and context usage through reactive listeners.
 * 
 * @author anahata
 */
public class AgisTableModel extends AbstractTableModel {

    /** The list of active agi sessions being tracked. */
    private final List<Agi> sessions = new ArrayList<>();
    /** The localized column names for the table. */
    private final String[] columnNames = {"Nickname", "ID", "Status", "Msgs", "Res", "Context %", "Summary"};
    
    /** The listener for metric changes in individual sessions. */
    private final PropertyChangeListener metricsListener = this::handleMetricsChange;
    /** The container configuration providing session data. */
    private final AbstractAsiContainer asiConfig;
    /** The listener for changes in the container's session list. */
    private final PropertyChangeListener asiListener = this::handleAsiChange;

    /** The column index for the session name. */
    public static final int SESSION_COL = 0;
    /** The column index for the session ID. */
    public static final int ID_COL = 1;
    /** The column index for the session status. */
    public static final int STATUS_COL = 2;
    /** The column index for the message count. */
    public static final int MESSAGES_COL = 3;
    /** The column index for the resource count. */
    public static final int RESOURCES_COL = 4;
    /** The column index for context window usage. */
    public static final int CONTEXT_COL = 5;
    /** The column index for the conversation summary. */
    public static final int SUMMARY_COL = 6;

    /** 
     * Constructs a new model and registers a listener on the provided container.
     * 
     * @param asiConfig The container to track.
     */
    public AgisTableModel(@NonNull AbstractAsiContainer asiConfig) {
        this.asiConfig = asiConfig;
        refresh();
        asiConfig.addPropertyChangeListener(asiListener);
    }

    /** 
     * {@inheritDoc} 
     */
    @Override
    public int getRowCount() {
        synchronized (sessions) {
            return sessions.size();
        }
    }

    /** 
     * {@inheritDoc} 
     */
    @Override
    public int getColumnCount() {
        return columnNames.length;
    }

    /** 
     * {@inheritDoc} 
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
        switch (columnIndex) {
            case MESSAGES_COL:
            case RESOURCES_COL:
                return Integer.class;
            case CONTEXT_COL:
                return Double.class;
            case STATUS_COL:
                return AgiStatus.class;
            default:
                return String.class;
        }
    }

    /** 
     * {@inheritDoc} 
     */
    @Override
    public Object getValueAt(int rowIndex, int columnIndex) {
        Agi agi;
        synchronized (sessions) {
            if (rowIndex < 0 || rowIndex >= sessions.size()) {
                return null;
            }
            agi = sessions.get(rowIndex);
        }

        switch (columnIndex) {
            case SESSION_COL:
                return agi.getDisplayName();
            case ID_COL:
                return agi.getConfig().getSessionId();
            case STATUS_COL:
                return agi.getStatusManager().getCurrentStatus();
            case MESSAGES_COL:
                return agi.getContextManager().getHistory().size();
            case RESOURCES_COL:
                return agi.getResourceManager().getResources().size();
            case CONTEXT_COL:
                return agi.getContextWindowUsage();
            case SUMMARY_COL:
                return agi.getConversationSummary();
            default:
                return null;
        }
    }

    /**
     * Refreshes the table model by synchronizing with the {@link AbstractAsiContainer}.
     * This method manages the attachment and detachment of per-session listeners.
     */
    public final void refresh() {
        List<Agi> activeAgis = asiConfig.getActiveAgis();
        
        synchronized (sessions) {
            // 1. Remove sessions that are no longer active
            for (int i = sessions.size() - 1; i >= 0; i--) {
                Agi agi = sessions.get(i);
                if (!activeAgis.contains(agi)) {
                    detachListeners(agi);
                    sessions.remove(i);
                    fireTableRowsDeleted(i, i);
                }
            }

            // 2. Add new active sessions
            for (int i = 0; i < activeAgis.size(); i++) {
                Agi agi = activeAgis.get(i);
                if (!sessions.contains(agi)) {
                    attachListeners(agi);
                    sessions.add(i, agi);
                    fireTableRowsInserted(i, i);
                }
            }

            // 3. Update existing rows to reflect order or potential non-reactive changes
            if (!sessions.isEmpty()) {
                fireTableRowsUpdated(0, sessions.size() - 1);
            }
        }
    }

    private void attachListeners(Agi agi) {
        agi.addPropertyChangeListener("nickname", metricsListener);
        agi.addPropertyChangeListener("summary", metricsListener);
        agi.addPropertyChangeListener("open", metricsListener);
        agi.getContextManager().addPropertyChangeListener("history", metricsListener);
        agi.getResourceManager().addPropertyChangeListener("resources", metricsListener);
        agi.getStatusManager().addPropertyChangeListener("currentStatus", metricsListener);
    }

    private void detachListeners(Agi agi) {
        agi.removePropertyChangeListener("nickname", metricsListener);
        agi.removePropertyChangeListener("summary", metricsListener);
        agi.removePropertyChangeListener("open", metricsListener);
        agi.getContextManager().removePropertyChangeListener("history", metricsListener);
        agi.getResourceManager().removePropertyChangeListener("resources", metricsListener);
        agi.getStatusManager().removePropertyChangeListener("currentStatus", metricsListener);
    }

    /** 
     * Handles property change events from individual sessions to update specific rows.
     * 
     * @param evt The property change event.
     */
    private void handleMetricsChange(PropertyChangeEvent evt) {
        Object source = evt.getSource();
        Agi targetAgi = null;
        
        if (source instanceof Agi a) {
            targetAgi = a;
        } else if (source instanceof uno.anahata.asi.agi.context.ContextManager cm) {
            targetAgi = cm.getAgi();
        } else if (source instanceof uno.anahata.asi.agi.resource.ResourceManager rm) {
            targetAgi = rm.getAgi();
        } else if (source instanceof uno.anahata.asi.agi.status.StatusManager sm) {
            targetAgi = sm.getAgi();
        }
        
        if (targetAgi != null) {
            synchronized (sessions) {
                int row = sessions.indexOf(targetAgi);
                if (row >= 0) {
                    fireTableRowsUpdated(row, row);
                }
            }
        }
    }

    /** 
     * Retrieves the agi session at the specified row index.
     * 
     * @param row The model row index.
     * @return The session, or null if the index is out of bounds.
     */
    public Agi getAgiAt(int row) {
        synchronized (sessions) {
            if (row >= 0 && row < sessions.size()) {
                return sessions.get(row);
            }
        }
        return null;
    }

    /** 
     * Handles property change events from the ASI container to trigger a refresh.
     * 
     * @param evt The property change event.
     */
    private void handleAsiChange(PropertyChangeEvent evt) {
        if ("activeAgis".equals(evt.getPropertyName())) {
            refresh();
        }
    }
    
    /** 
     * Unregisters the listener from the container and cleans up the model.
     */
    public void dispose() {
        asiConfig.removePropertyChangeListener(asiListener);
        synchronized (sessions) {
            for (Agi agi : sessions) {
                detachListeners(agi);
            }
        }
    }
}
