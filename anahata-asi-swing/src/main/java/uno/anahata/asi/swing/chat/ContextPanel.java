/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Dimension;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Set;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.tree.TreePath;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.JXTreeTable;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.swing.chat.tool.ToolPanel;
import uno.anahata.asi.swing.chat.tool.ToolkitPanel;
import uno.anahata.asi.swing.chat.tool.ContextProviderPanel;
import uno.anahata.asi.swing.chat.tool.ContextTreeTableModel;
import uno.anahata.asi.swing.chat.tool.AbstractContextNode;
import uno.anahata.asi.swing.chat.tool.ProviderNode;
import uno.anahata.asi.swing.chat.tool.ToolkitNode;
import uno.anahata.asi.swing.chat.tool.ToolNode;
import uno.anahata.asi.swing.chat.tool.MessageNode;
import uno.anahata.asi.swing.chat.tool.PartNode;
import uno.anahata.asi.swing.chat.tool.ResourceNode;
import uno.anahata.asi.swing.chat.tool.ResourcePanel;
import uno.anahata.asi.swing.chat.tool.ResourcesNode;
import uno.anahata.asi.swing.chat.tool.ContextTreeCellRenderer;
import uno.anahata.asi.swing.chat.render.MessagePanelFactory;
import uno.anahata.asi.swing.chat.render.PartPanelFactory;
import uno.anahata.asi.swing.icons.RestartIcon;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;

/**
 * A panel dedicated to displaying and managing the available AI context
 * (history, tools, providers, and resources) using a hierarchical JXTreeTable.
 * <p>
 * This panel provides a JNDI-style view of the entire AI context. It uses 
 * a split-pane layout with a tree table on the left and a dynamic detail 
 * area on the right that switches panels based on the selected node type.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ContextPanel extends JPanel {

    /** The parent chat panel. */
    private final ChatPanel chatPanel;
    /** The active chat session. */
    private Chat chat;
    /** The tree table component for the context hierarchy. */
    private final JXTreeTable treeTable;
    /** The model for the tree table. */
    private ContextTreeTableModel treeTableModel;
    
    /** Container for the detail panels, using CardLayout for switching. */
    private final JPanel detailContainer;
    /** Layout for switching between detail panels. */
    private final CardLayout detailLayout;
    
    /** Panel for displaying tool details. */
    private final ToolPanel toolPanel;
    /** Panel for displaying toolkit details. */
    private final ToolkitPanel toolkitPanel;
    /** Panel for displaying context provider details. */
    private final ContextProviderPanel providerPanel;
    /** Panel for displaying resource details. */
    private final ResourcePanel resourcePanel;
    /** Container for dynamically created message or part panels. */
    private final JPanel messagePartDetailPanel;
    
    /** Listener for history changes to trigger tree refreshes. */
    private EdtPropertyChangeListener historyListener;

    /**
     * Constructs a new ContextPanel.
     * @param chatPanel The parent chat panel.
     */
    public ContextPanel(@NonNull ChatPanel chatPanel) {
        this.chatPanel = chatPanel;
        this.chat = chatPanel.getChat();
        this.treeTableModel = new ContextTreeTableModel(chat);
        this.treeTable = new JXTreeTable();
        
        this.detailLayout = new CardLayout();
        this.detailContainer = new JPanel(detailLayout);
        
        this.toolPanel = new ToolPanel(this);
        this.toolkitPanel = new ToolkitPanel(this);
        this.providerPanel = new ContextProviderPanel(this);
        this.resourcePanel = new ResourcePanel(this);
        this.messagePartDetailPanel = new JPanel(new BorderLayout());
        
        detailContainer.add(toolPanel, "tool");
        detailContainer.add(toolkitPanel, "toolkit");
        detailContainer.add(providerPanel, "provider");
        detailContainer.add(resourcePanel, "resource");
        detailContainer.add(new JScrollPane(messagePartDetailPanel), "messagePart");
        detailContainer.add(new JPanel(), "empty");
        
        setLayout(new BorderLayout());
        
        // Automatically refresh the tree when the history changes
        this.historyListener = new EdtPropertyChangeListener(this, chat.getContextManager(), "history", evt -> refresh());
    }

    /**
     * Gets the parent chat panel.
     * @return The chat panel.
     */
    public ChatPanel getChatPanel() {
        return chatPanel;
    }

    /**
     * Gets the active chat session.
     * @return The chat session.
     */
    public Chat getChat() {
        return chat;
    }

    /**
     * Initializes the components and layout of the panel.
     */
    public void initComponents() {
        // Configure Toolbar
        JToolBar toolBar = new JToolBar();
        toolBar.setFloatable(false);
        
        JButton refreshButton = new JButton("Refresh Tokens", new RestartIcon(16));
        refreshButton.setToolTipText("Recalculate token counts for all context items (Snapshot)");
        refreshButton.addActionListener(e -> refreshTokens());
        toolBar.add(refreshButton);
        
        add(toolBar, BorderLayout.NORTH);

        // Configure TreeTable
        treeTable.setTreeTableModel(treeTableModel);
        treeTable.setColumnControlVisible(true);
        treeTable.setEditable(false);
        treeTable.setRootVisible(false);
        treeTable.setShowsRootHandles(true);
        treeTable.setTreeCellRenderer(new ContextTreeCellRenderer());
        
        // Disable auto-resize to respect preferred widths
        treeTable.setAutoResizeMode(JXTreeTable.AUTO_RESIZE_OFF);
        
        applyColumnWidths();

        // Configure Split Pane
        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, new JScrollPane(treeTable), detailContainer);
        splitPane.setResizeWeight(0.3);
        add(splitPane, BorderLayout.CENTER);

        // Add a TreeSelectionListener to update the detailPanel
        treeTable.getTreeSelectionModel().addTreeSelectionListener((TreeSelectionEvent e) -> {
            TreePath path = e.getNewLeadSelectionPath();
            Object node = (path != null) ? path.getLastPathComponent() : null;
            
            if (node instanceof AbstractContextNode<?> cn) {
                if (cn instanceof ToolNode tn) {
                    toolPanel.setTool(tn.getUserObject());
                    detailLayout.show(detailContainer, "tool");
                } else if (cn instanceof ToolkitNode tkn) {
                    toolkitPanel.setToolkit(tkn.getUserObject());
                    detailLayout.show(detailContainer, "toolkit");
                } else if (cn instanceof ProviderNode pn) {
                    providerPanel.setContextProvider(pn.getUserObject());
                    detailLayout.show(detailContainer, "provider");
                } else if (cn instanceof MessageNode mn) {
                    updateMessagePartDetail(MessagePanelFactory.createMessagePanel(chatPanel, mn.getUserObject()));
                    detailLayout.show(detailContainer, "messagePart");
                } else if (cn instanceof PartNode pn) {
                    updateMessagePartDetail(PartPanelFactory.createPartPanel(chatPanel, pn.getUserObject()));
                    detailLayout.show(detailContainer, "messagePart");
                } else if (cn instanceof ResourceNode rn) {
                    resourcePanel.setResource(rn.getUserObject());
                    detailLayout.show(detailContainer, "resource");
                } else {
                    detailLayout.show(detailContainer, "empty");
                }
            } else {
                detailLayout.show(detailContainer, "empty");
            }
        });
        
        SwingUtilities.invokeLater(() -> {
            splitPane.setDividerLocation(0.3);
            refresh();
            refreshTokens(); // Initial token refresh
        });
    }

    /**
     * Applies the standard column widths to the tree table.
     */
    private void applyColumnWidths() {
        // Set preferred width for the first column (Name) - Increased to 500
        treeTable.getColumnModel().getColumn(0).setPreferredWidth(500);
        treeTable.getColumnModel().getColumn(0).setMinWidth(200);
    }

    /**
     * Updates the message/part detail area with the given panel.
     * @param panel The panel to display.
     */
    private void updateMessagePartDetail(JPanel panel) {
        messagePartDetailPanel.removeAll();
        if (panel != null) {
            messagePartDetailPanel.add(panel, BorderLayout.NORTH);
        }
        messagePartDetailPanel.revalidate();
        messagePartDetailPanel.repaint();
    }

    /**
     * Reloads the panel with the new chat state.
     */
    public void reload() {
        this.chat = chatPanel.getChat();
        
        if (historyListener != null) {
            historyListener.unbind();
        }
        this.historyListener = new EdtPropertyChangeListener(this, chat.getContextManager(), "history", evt -> refresh());

        this.treeTableModel = new ContextTreeTableModel(chat);
        this.treeTable.setTreeTableModel(treeTableModel);
        refresh();
        refreshTokens();
    }

    /**
     * Refreshes the data in the tree table while preserving expansion state.
     */
    public final void refresh() {
        SwingUtilities.invokeLater(() -> {
            log.info("Refreshing ContextPanel tree for chat: {}", chat.getShortId());
            
            // Save expansion state
            Set<TreePath> expandedPaths = getExpandedPaths();

            treeTableModel.refresh();
            
            // Restore expansion state
            restoreExpandedPaths(expandedPaths);
            
            applyColumnWidths();
            
            // Select the first node if nothing is selected
            if (treeTable.getSelectedRow() == -1 && treeTable.getRowCount() > 0) {
                treeTable.setRowSelectionInterval(0, 0);
            }
            
            treeTable.revalidate();
            treeTable.repaint();
        });
    }

    /**
     * Captures the current expansion state of the tree.
     * @return A set of expanded TreePaths.
     */
    private Set<TreePath> getExpandedPaths() {
        Set<TreePath> expandedPaths = new HashSet<>();
        for (int i = 0; i < treeTable.getRowCount(); i++) {
            if (treeTable.isExpanded(i)) {
                expandedPaths.add(treeTable.getPathForRow(i));
            }
        }
        return expandedPaths;
    }

    /**
     * Restores the expansion state of the tree.
     * @param expandedPaths The set of paths to expand.
     */
    private void restoreExpandedPaths(Set<TreePath> expandedPaths) {
        for (TreePath path : expandedPaths) {
            treeTable.expandPath(path);
        }
    }
    
    /**
     * Triggers a background recalculation of token counts.
     */
    public void refreshTokens() {
        chat.getExecutor().execute(() -> {
            try {
                // Save expansion state on EDT
                final Set<TreePath> expandedPaths = new HashSet<>();
                SwingUtilities.invokeAndWait(() -> expandedPaths.addAll(getExpandedPaths()));

                treeTableModel.refreshTokens();
                
                SwingUtilities.invokeLater(() -> {
                    // Restore expansion state
                    restoreExpandedPaths(expandedPaths);
                    
                    applyColumnWidths();
                    treeTable.repaint();
                });
            } catch (Exception e) {
                log.error("Error refreshing context tokens", e);
            }
        });
    }

    /**
     * {@inheritDoc}
     * Triggers an initial refresh when the component is added to the UI.
     */
    @Override
    public void addNotify() {
        super.addNotify();
        refresh();
    }
}
