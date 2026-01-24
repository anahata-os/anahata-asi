/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.text.DecimalFormat;
import java.util.Comparator;
import java.util.List;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableRowSorter;
import lombok.NonNull;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.status.ChatStatus;

/**
 * A container panel that displays active AI chat sessions in a tabular format.
 * This implementation is ideal for environments with limited vertical space,
 * such as the NetBeans Output or Navigator areas.
 * 
 * @author anahata
 */
public class AsiTableContainerPanel extends AbstractAsiContainerPanel {

    private final JTable table;
    private final ChatsTableModel model;

    /**
     * Constructs a new table container panel.
     * 
     * @param container The ASI container.
     */
    public AsiTableContainerPanel(@NonNull AsiContainer container) {
        super(container);
        
        this.model = new ChatsTableModel(container);
        this.table = new JTable(model);

        table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        table.setFillsViewportHeight(true);

        // Custom Renderers
        table.setDefaultRenderer(ChatStatus.class, new StatusCellRenderer());
        table.getColumnModel().getColumn(ChatsTableModel.CONTEXT_COL).setCellRenderer(new ContextUsageCellRenderer());

        // Sorting
        TableRowSorter<ChatsTableModel> sorter = new TableRowSorter<>(model);
        table.setRowSorter(sorter);
        sorter.setComparator(ChatsTableModel.CONTEXT_COL, Comparator.comparingDouble(d -> (Double) d));
        sorter.setSortKeys(List.of(new javax.swing.RowSorter.SortKey(ChatsTableModel.SESSION_COL, javax.swing.SortOrder.ASCENDING)));

        table.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 2) {
                    Chat chat = getSelectedChat();
                    if (chat != null) {
                        focus(chat);
                    }
                }
            }
        });

        table.getSelectionModel().addListSelectionListener(e -> {
            if (!e.getValueIsAdjusting()) {
                updateButtonState();
            }
        });

        add(new JScrollPane(table), BorderLayout.CENTER);
        setColumnWidths();
    }

    @Override
    protected void refreshView() {
        model.refresh();
    }

    @Override
    public Chat getSelectedChat() {
        int viewRow = table.getSelectedRow();
        if (viewRow >= 0) {
            int modelRow = table.convertRowIndexToModel(viewRow);
            return model.getChatAt(modelRow);
        }
        return null;
    }

    private void setColumnWidths() {
        TableColumn statusColumn = table.getColumnModel().getColumn(ChatsTableModel.STATUS_COL);
        statusColumn.setMinWidth(120);
        statusColumn.setMaxWidth(150);

        TableColumn msgColumn = table.getColumnModel().getColumn(ChatsTableModel.MESSAGES_COL);
        msgColumn.setMinWidth(60);
        msgColumn.setMaxWidth(80);

        TableColumn ctxColumn = table.getColumnModel().getColumn(ChatsTableModel.CONTEXT_COL);
        ctxColumn.setMinWidth(80);
        ctxColumn.setMaxWidth(100);
    }

    /**
     * A cell renderer for displaying {@link ChatStatus} with appropriate colors.
     */
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

    /**
     * A cell renderer for displaying context window usage as a percentage.
     */
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
