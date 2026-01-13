/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.tree.TreePath;
import lombok.Getter;
import lombok.NonNull;
import org.jdesktop.swingx.JXTreeTable;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.tool.AbstractTool;
import uno.anahata.ai.model.tool.AbstractToolkit;
import uno.anahata.ai.swing.chat.tool.ToolDetailPanel;
import uno.anahata.ai.swing.chat.tool.ToolkitDetailPanel;
import uno.anahata.ai.swing.chat.tool.ToolkitTreeTableModel;

/**
 * A panel dedicated to displaying and managing the available AI tools (functions)
 * using a hierarchical JXTreeTable. This panel acts as a container, delegating
 * the detail view to specialized panels for Tools and Toolkits.
 *
 * @author anahata
 */
@Getter
public class ToolsPanel extends JPanel {

    private final ChatPanel chatPanel;
    private Chat chat;
    private final JXTreeTable treeTable;
    private ToolkitTreeTableModel treeTableModel;
    
    private final JPanel detailContainer;
    private final CardLayout detailLayout;
    private final ToolDetailPanel toolDetailPanel;
    private final ToolkitDetailPanel toolkitDetailPanel;

    public ToolsPanel(@NonNull ChatPanel chatPanel) {
        this.chatPanel = chatPanel;
        this.chat = chatPanel.getChat();
        this.treeTableModel = new ToolkitTreeTableModel(chat.getToolManager());
        this.treeTable = new JXTreeTable(treeTableModel);
        
        this.detailLayout = new CardLayout();
        this.detailContainer = new JPanel(detailLayout);
        
        this.toolDetailPanel = new ToolDetailPanel(this);
        this.toolkitDetailPanel = new ToolkitDetailPanel(this);
        
        detailContainer.add(toolDetailPanel, "tool");
        detailContainer.add(toolkitDetailPanel, "toolkit");
        detailContainer.add(new JPanel(), "empty");
        
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
        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, new JScrollPane(treeTable), detailContainer);
        splitPane.setDividerLocation(400);
        add(splitPane, BorderLayout.CENTER);

        // Add a TreeSelectionListener to update the detailPanel
        treeTable.getTreeSelectionModel().addTreeSelectionListener((TreeSelectionEvent e) -> {
            TreePath path = e.getNewLeadSelectionPath();
            Object node = (path != null) ? path.getLastPathComponent() : null;
            
            if (node instanceof AbstractToolkit toolkit) {
                toolkitDetailPanel.setToolkit(toolkit);
                detailLayout.show(detailContainer, "toolkit");
            } else if (node instanceof AbstractTool tool) {
                toolDetailPanel.setTool(tool);
                detailLayout.show(detailContainer, "tool");
            } else {
                detailLayout.show(detailContainer, "empty");
            }
        });
        
        refresh();
    }

    /**
     * Reloads the panel with the new chat state.
     */
    public void reload() {
        this.chat = chatPanel.getChat();
        this.treeTableModel = new ToolkitTreeTableModel(chat.getToolManager());
        this.treeTable.setTreeTableModel(treeTableModel);
        refresh();
    }

    /**
     * Refreshes the data in the tree table. This is called by the detail panels
     * when a setting (like a permission or enabled state) is changed.
     */
    public final void refresh() {
        treeTableModel.refresh();
        treeTable.expandAll();
    }
}
