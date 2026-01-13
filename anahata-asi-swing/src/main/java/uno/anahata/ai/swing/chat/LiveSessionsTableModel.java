/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;
import javax.swing.table.AbstractTableModel;
import lombok.NonNull;
import uno.anahata.ai.AsiConfig;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.status.ChatStatus;

/**
 * A reusable table model for displaying active AI chat sessions.
 * This model tracks the {@link AsiConfig} and provides real-time updates
 * on session status, message count, and context usage.
 * 
 * @author gemini-3-flash-preview
 */
public class LiveSessionsTableModel extends AbstractTableModel {

    private final List<Chat> sessions = new ArrayList<>();
    private final String[] columnNames = {"Nickname", "ID", "Status", "Msgs", "Context %"};
    private final AsiConfig asiConfig;
    private final PropertyChangeListener asiListener = this::handleAsiChange;

    public static final int SESSION_COL = 0;
    public static final int ID_COL = 1;
    public static final int STATUS_COL = 2;
    public static final int MESSAGES_COL = 3;
    public static final int CONTEXT_COL = 4;

    public LiveSessionsTableModel(@NonNull AsiConfig asiConfig) {
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
                return ChatStatus.class;
            default:
                return String.class;
        }
    }

    @Override
    public Object getValueAt(int rowIndex, int columnIndex) {
        if (rowIndex < 0 || rowIndex >= sessions.size()) {
            return null;
        }
        Chat chat = sessions.get(rowIndex);

        switch (columnIndex) {
            case SESSION_COL:
                return chat.getNickname();
            case ID_COL:
                return chat.getShortId();
            case STATUS_COL:
                return chat.getStatusManager().getCurrentStatus();
            case MESSAGES_COL:
                return chat.getContextManager().getHistory().size();
            case CONTEXT_COL:
                return chat.getContextWindowUsage();
            default:
                return null;
        }
    }

    /**
     * Refreshes the table model by synchronizing with the {@link AsiConfig}.
     */
    public final void refresh() {
        List<Chat> activeChats = asiConfig.getActiveChats();
        
        // Identify removed sessions
        for (int i = sessions.size() - 1; i >= 0; i--) {
            if (!activeChats.contains(sessions.get(i))) {
                sessions.remove(i);
                fireTableRowsDeleted(i, i);
            }
        }

        // Identify added sessions
        for (int i = 0; i < activeChats.size(); i++) {
            Chat chat = activeChats.get(i);
            if (!sessions.contains(chat)) {
                sessions.add(i, chat);
                fireTableRowsInserted(i, i);
            }
        }

        // Identify updated rows
        if (!sessions.isEmpty()) {
            fireTableRowsUpdated(0, sessions.size() - 1);
        }
    }

    public Chat getChatAt(int row) {
        if (row >= 0 && row < sessions.size()) {
            return sessions.get(row);
        }
        return null;
    }

    private void handleAsiChange(PropertyChangeEvent evt) {
        if ("activeChats".equals(evt.getPropertyName())) {
            refresh();
        }
    }
    
    public void dispose() {
        asiConfig.removePropertyChangeListener(asiListener);
    }
}
