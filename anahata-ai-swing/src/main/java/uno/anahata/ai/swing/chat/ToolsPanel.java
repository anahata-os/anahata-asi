/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.BorderLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.tree.TreePath;
import lombok.Getter;
import org.jdesktop.swingx.JXTreeTable;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.swing.chat.tool.ToolDetailPanel;
import uno.anahata.ai.swing.chat.tool.ToolkitTreeTableModel;

/**
 * A panel dedicated to displaying and managing the available AI tools (functions)
 * using a hierarchical JXTreeTable. This panel acts as a container, delegating
 * the detail view to the specialized ToolDetailPanel.
 *
 * @author pablo
 */
@Getter
public class ToolsPanel extends JPanel {

    private final Chat chat;
    private final JXTreeTable treeTable;
    private final ToolkitTreeTableModel treeTableModel;
    private final ToolDetailPanel toolDetailPanel;

    public ToolsPanel(Chat chat) {
        this.chat = chat;
        this.treeTableModel = new ToolkitTreeTableModel(chat.getToolManager());
        this.treeTable = new JXTreeTable(treeTableModel);
        this.toolDetailPanel = new ToolDetailPanel(this);
        setLayout(new BorderLayout());
    }

    /**
     * Initializes the components and layout of the panel.
     */
    public void initComponents() {
        // Configure TreeTable
        treeTable.setColumnControlVisible(true);
        treeTable.setEditable(false);
        treeTable.expandAll();

        // Configure Split Pane
        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, new JScrollPane(treeTable), new JScrollPane(toolDetailPanel));
        splitPane.setDividerLocation(600);
        add(splitPane, BorderLayout.CENTER);

        // Add a TreeSelectionListener to update the detailPanel
        treeTable.getTreeSelectionModel().addTreeSelectionListener((TreeSelectionEvent e) -> {
            TreePath path = e.getNewLeadSelectionPath();
            Object node = (path != null) ? path.getLastPathComponent() : null;
            toolDetailPanel.setNode(node);
        });
        
        refresh();
    }

    /**
     * Refreshes the data in the tree table. This is called by the detail panel
     * when a setting (like a permission) is changed.
     */
    public final void refresh() {
        treeTableModel.refresh();
        treeTable.expandAll();
    }
}
