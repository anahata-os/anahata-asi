/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara!
 */
package uno.anahata.ai.swing.chat;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.text.DecimalFormat;
import java.util.Comparator;
import java.util.List;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JToolBar;
import javax.swing.ListSelectionModel;
import javax.swing.Timer;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableRowSorter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.status.ChatStatus;
import uno.anahata.ai.swing.chat.SwingChatConfig;
import uno.anahata.ai.swing.icons.DeleteIcon;
import uno.anahata.ai.swing.icons.RestartIcon;

/**
 * A reusable Swing panel for managing active AI chat sessions.
 * This panel provides a table view of all live sessions and actions to create,
 * focus, close, or dispose of them.
 * 
 * @author anahata-gemini-pro-2.5
 */
public class LiveSessionsPanel extends JPanel {

    private final JTable table;
    private final LiveSessionsTableModel model;
    private final Timer refreshTimer;
    private final JButton closeButton;
    private final JButton disposeButton;
    
    @Setter
    private SessionController controller;

    public LiveSessionsPanel() {
        setLayout(new BorderLayout());

        // Toolbar for Actions
        JToolBar toolBar = new JToolBar();
        toolBar.setFloatable(false);

        JButton newButton = new JButton("New", new RestartIcon(16));
        newButton.setToolTipText("Create a new AI session");
        newButton.addActionListener(e -> {
            if (controller != null) controller.createNew();
        });
        toolBar.add(newButton);

        closeButton = new JButton("Close");
        closeButton.setToolTipText("Close the selected AI session window");
        closeButton.addActionListener(e -> {
            Chat chat = getSelectedChat();
            if (chat != null && controller != null) controller.close(chat);
        });
        closeButton.setEnabled(false);
        toolBar.add(closeButton);
        
        toolBar.add(Box.createHorizontalGlue());

        disposeButton = new JButton("Dispose", new DeleteIcon(16));
        disposeButton.setToolTipText("Permanently dispose of the selected session");
        disposeButton.addActionListener(e -> {
            Chat chat = getSelectedChat();
            if (chat != null && controller != null) controller.dispose(chat);
        });
        disposeButton.setEnabled(false);
        toolBar.add(disposeButton);

        add(toolBar, BorderLayout.NORTH);

        model = new LiveSessionsTableModel();
        table = new JTable(model);
        table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        table.setFillsViewportHeight(true);
        
        table.getSelectionModel().addListSelectionListener(e -> {
            if (!e.getValueIsAdjusting()) {
                updateButtonState();
            }
        });

        // Custom Renderers
        table.setDefaultRenderer(ChatStatus.class, new StatusCellRenderer());
        table.getColumnModel().getColumn(LiveSessionsTableModel.CONTEXT_COL).setCellRenderer(new ContextUsageCellRenderer());

        // Sorting
        TableRowSorter<LiveSessionsTableModel> sorter = new TableRowSorter<>(model);
        table.setRowSorter(sorter);
        sorter.setComparator(LiveSessionsTableModel.CONTEXT_COL, Comparator.comparingDouble(d -> (Double) d));
        sorter.setSortKeys(List.of(new javax.swing.RowSorter.SortKey(LiveSessionsTableModel.SESSION_COL, javax.swing.SortOrder.ASCENDING)));

        table.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 2) {
                    Chat chat = getSelectedChat();
                    if (chat != null && controller != null) {
                        controller.focus(chat);
                    }
                }
            }
        });

        add(new JScrollPane(table), BorderLayout.CENTER);

        refreshTimer = new Timer(1000, e -> model.refresh());
        
        setColumnWidths();
    }

    private void setColumnWidths() {
        TableColumn statusColumn = table.getColumnModel().getColumn(LiveSessionsTableModel.STATUS_COL);
        statusColumn.setMinWidth(120);
        statusColumn.setMaxWidth(150);

        TableColumn msgColumn = table.getColumnModel().getColumn(LiveSessionsTableModel.MESSAGES_COL);
        msgColumn.setMinWidth(60);
        msgColumn.setMaxWidth(80);

        TableColumn ctxColumn = table.getColumnModel().getColumn(LiveSessionsTableModel.CONTEXT_COL);
        ctxColumn.setMinWidth(80);
        ctxColumn.setMaxWidth(100);
    }

    public void startRefresh() {
        refreshTimer.start();
    }

    public void stopRefresh() {
        refreshTimer.stop();
    }

    private Chat getSelectedChat() {
        int viewRow = table.getSelectedRow();
        if (viewRow >= 0) {
            int modelRow = table.convertRowIndexToModel(viewRow);
            return model.getChatAt(modelRow);
        }
        return null;
    }
    
    private void updateButtonState() {
        Chat selected = getSelectedChat();
        boolean isSelected = selected != null;
        disposeButton.setEnabled(isSelected);
        closeButton.setEnabled(isSelected);
    }

    /**
     * Interface for controlling session lifecycle from the host application.
     */
    public interface SessionController {
        void focus(@NonNull Chat chat);
        void close(@NonNull Chat chat);
        void dispose(@NonNull Chat chat);
        void createNew();
    }

    private static class StatusCellRenderer extends DefaultTableCellRenderer {
        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
            Component c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
            if (value instanceof ChatStatus) {
                ChatStatus status = (ChatStatus) value;
                c.setForeground(SwingChatConfig.getColor(status));
                setText(status.getDisplayName());
            }
            return c;
        }
    }

    private static class ContextUsageCellRenderer extends DefaultTableCellRenderer {
        private final DecimalFormat PERCENT_FORMAT = new DecimalFormat("0.0%");

        public ContextUsageCellRenderer() {
            setHorizontalAlignment(JLabel.RIGHT);
        }

        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

            if (value instanceof Double) {
                double percentage = (Double) value;
                setText(PERCENT_FORMAT.format(percentage));
                setForeground(SwingChatConfig.getColorForContextUsage(percentage));
            }
            return this;
        }
    }
}
