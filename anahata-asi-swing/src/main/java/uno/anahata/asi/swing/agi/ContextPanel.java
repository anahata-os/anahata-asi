/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.table.TableColumn;
import javax.swing.tree.TreePath;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.JXTreeTable;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.swing.agi.tool.ToolPanel;
import uno.anahata.asi.swing.agi.tool.ToolkitPanel;
import uno.anahata.asi.swing.agi.tool.ContextProviderPanel;
import uno.anahata.asi.swing.agi.tool.ContextTreeTableModel;
import uno.anahata.asi.swing.agi.tool.AbstractContextNode;
import uno.anahata.asi.swing.agi.tool.ProviderNode;
import uno.anahata.asi.swing.agi.tool.ToolkitNode;
import uno.anahata.asi.swing.agi.tool.ToolNode;
import uno.anahata.asi.swing.agi.tool.MessageNode;
import uno.anahata.asi.swing.agi.tool.PartNode;
import uno.anahata.asi.swing.agi.tool.ResourceNode;
import uno.anahata.asi.swing.agi.tool.ResourcePanel;
import uno.anahata.asi.swing.agi.tool.ResourcesNode;
import uno.anahata.asi.swing.agi.tool.ToolsNode;
import uno.anahata.asi.swing.agi.tool.ContextTreeCellRenderer;
import uno.anahata.asi.swing.agi.render.MessagePanelFactory;
import uno.anahata.asi.swing.agi.render.PartPanelFactory;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.RestartIcon;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.tool.ToolManager;

/**
 * A panel dedicated to displaying and managing the available AI context
 * (history, tools, providers, and resources) using a hierarchical JXTreeTable.
 * <p>
 * This panel provides a JNDI-style view of the entire AI context. It uses 
 * a split-pane layout with a tree table on the left and a detail 
 * area on the right that switches panels based on the selected node type.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ContextPanel extends JPanel {

    /** The parent agi panel. */
    private final AgiPanel agiPanel;
    /** The active agi session. */
    private Agi agi;
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
    
    /** Registry for mapping domain types to custom UI configurers. */
    private final Map<Class<?>, BiConsumer<Object, JPanel>> customConfigRegistry = new HashMap<>();

    /** Listener for history changes to trigger tree refreshes. */
    private EdtPropertyChangeListener historyListener;
    
    /** Listener for resource changes to trigger tree refreshes. */
    private EdtPropertyChangeListener resourcesListener;

    /**
     * Constructs a new ContextPanel.
     * @param agiPanel The parent agi panel.
     */
    public ContextPanel(@NonNull AgiPanel agiPanel) {
        this.agiPanel = agiPanel;
        this.agi = agiPanel.getAgi();
        this.treeTableModel = new ContextTreeTableModel(agiPanel);
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
        
        setupListeners();
        registerDefaultCustomConfigs();
    }
    
    /**
     * Registers the default configuration UIs for core components.
     */
    private void registerDefaultCustomConfigs() {
        registerCustomConfig(ToolManager.class, (tm, panel) -> {
            JCheckBox wrapCheck = new JCheckBox("Wrap Response Schemas");
            wrapCheck.setToolTipText("If enabled, tool response schemas include the full JavaMethodToolResponse structure. If disabled, only the raw return type is shown.");
            wrapCheck.setSelected(tm.isWrapResponseSchemas());
            wrapCheck.addActionListener(e -> tm.setWrapResponseSchemas(wrapCheck.isSelected()));
            panel.add(wrapCheck);
        });
    }

    /**
     * Registers a custom UI configurer for a specific domain object type.
     * 
     * @param <T> The type of the domain object.
     * @param type The class of the domain object.
     * @param configurer A consumer that populates a provided JPanel based on the object instance.
     */
    public <T> void registerCustomConfig(Class<T> type, BiConsumer<T, JPanel> configurer) {
        customConfigRegistry.put(type, (obj, panel) -> configurer.accept(type.cast(obj), panel));
    }

    /**
     * Populates a panel with custom configuration UI for a given object, if registered.
     * 
     * @param obj The domain object (user object of the selected node).
     * @param panel The target panel to populate.
     */
    public void populateCustomConfig(Object obj, JPanel panel) {
        panel.removeAll();
        if (obj == null) return;
        
        // Check for exact class match or interfaces/superclasses
        for (Map.Entry<Class<?>, BiConsumer<Object, JPanel>> entry : customConfigRegistry.entrySet()) {
            if (entry.getKey().isInstance(obj)) {
                entry.getValue().accept(obj, panel);
            }
        }
        panel.revalidate();
        panel.repaint();
    }

    /**
     * Sets up property change listeners for the current agi session.
     */
    private void setupListeners() {
        if (historyListener != null) {
            historyListener.unbind();
        }
        if (resourcesListener != null) {
            resourcesListener.unbind();
        }
        
        this.historyListener = new EdtPropertyChangeListener(this, agi.getContextManager(), "history", evt -> refresh(false));
        this.resourcesListener = new EdtPropertyChangeListener(this, agi.getResourceManager(), "resources", evt -> refresh(false));
    }

    /**
     * Gets the parent agi panel.
     * @return The agi panel.
     */
    public AgiPanel getAgiPanel() {
        return agiPanel;
    }

    /**
     * Gets the active agi session.
     * @return The agi session.
     */
    public Agi getAgi() {
        return agi;
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
        
        // Ensure manual resizing is preserved by disabling auto-creation of columns on model changes
        treeTable.setAutoCreateColumnsFromModel(false);
        
        applyColumnWidths();

        // Configure Split Pane
        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, new JScrollPane(treeTable), detailContainer);
        splitPane.setResizeWeight(0.5);
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
                } else if (cn instanceof ToolsNode tsn) {
                    toolkitPanel.setToolkit(tsn.getUserObject());
                    detailLayout.show(detailContainer, "toolkit");
                } else if (cn instanceof ProviderNode pn) {
                    providerPanel.setContextProvider(pn.getUserObject());
                    detailLayout.show(detailContainer, "provider");
                } else if (cn instanceof MessageNode mn) {
                    updateMessagePartDetail(MessagePanelFactory.createMessagePanel(agiPanel, mn.getUserObject()));
                    detailLayout.show(detailContainer, "messagePart");
                } else if (cn instanceof PartNode pn) {
                    updateMessagePartDetail(PartPanelFactory.createPartPanel(agiPanel, pn.getUserObject()));
                    detailLayout.show(detailContainer, "messagePart");
                } else if (cn instanceof ResourceNode rn) {
                    resourcePanel.setResource(rn.getUserObject());
                    detailLayout.show(detailContainer, "resource");
                } else if (cn instanceof ResourcesNode rsn) {
                    providerPanel.setContextProvider(rsn.getUserObject());
                    detailLayout.show(detailContainer, "provider");
                } else {
                    detailLayout.show(detailContainer, "empty");
                }
            } else {
                detailLayout.show(detailContainer, "empty");
            }
        });
        
        // Setup Popup Menu and Double-Click
        setupInteractions();
        
        SwingUtilities.invokeLater(() -> {
            splitPane.setDividerLocation(0.5);
            refresh(true);
        });
    }

    /**
     * Configures the popup menu and double-click behavior for the tree table.
     */
    private void setupInteractions() {
        JPopupMenu popup = new JPopupMenu();
        
        JMenuItem openInEditorItem = new JMenuItem("Open in Editor");
        openInEditorItem.addActionListener(e -> {
            int row = treeTable.getSelectedRow();
            if (row != -1) {
                Object node = treeTable.getPathForRow(row).getLastPathComponent();
                if (node instanceof ResourceNode rn) {
                    openResourceInEditor(rn.getUserObject());
                }
            }
        });
        
        JMenuItem removeItem = new JMenuItem("Remove from Context", new DeleteIcon(16));
        removeItem.addActionListener(e -> {
            int row = treeTable.getSelectedRow();
            if (row != -1) {
                Object node = treeTable.getPathForRow(row).getLastPathComponent();
                if (node instanceof ResourceNode rn) {
                    agi.getResourceManager().unregister(rn.getUserObject().getId());
                }
            }
        });
        
        JMenuItem toggleItem = new JMenuItem("Toggle Providing");
        toggleItem.addActionListener(e -> {
            int row = treeTable.getSelectedRow();
            if (row != -1) {
                Object node = treeTable.getPathForRow(row).getLastPathComponent();
                if (node instanceof ProviderNode pn) {
                    pn.getUserObject().setProviding(!pn.getUserObject().isProviding());
                } else if (node instanceof ToolkitNode tkn) {
                    tkn.getUserObject().setEnabled(!tkn.getUserObject().isEnabled());
                } else if (node instanceof ResourceNode rn) {
                    rn.getUserObject().setProviding(!rn.getUserObject().isProviding());
                }
                refresh(false);
            }
        });

        popup.add(openInEditorItem);
        popup.add(removeItem);
        popup.addSeparator();
        popup.add(toggleItem);

        treeTable.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 2 && SwingUtilities.isLeftMouseButton(e)) {
                    int row = treeTable.rowAtPoint(e.getPoint());
                    if (row != -1) {
                        Object node = treeTable.getPathForRow(row).getLastPathComponent();
                        if (node instanceof ResourceNode rn) {
                            openResourceInEditor(rn.getUserObject());
                        }
                    }
                }
            }
            
            @Override
            public void mousePressed(MouseEvent e) {
                showPopup(e);
            }
            
            @Override
            public void mouseReleased(MouseEvent e) {
                showPopup(e);
            }
            
            private void showPopup(MouseEvent e) {
                if (e.isPopupTrigger()) {
                    int row = treeTable.rowAtPoint(e.getPoint());
                    if (row != -1) {
                        treeTable.setRowSelectionInterval(row, row);
                        Object node = treeTable.getPathForRow(row).getLastPathComponent();
                        
                        boolean isResource = node instanceof ResourceNode;
                        boolean isToolkit = node instanceof ToolkitNode;
                        boolean isProvider = node instanceof ProviderNode;
                        
                        openInEditorItem.setVisible(isResource);
                        removeItem.setVisible(isResource);
                        
                        toggleItem.setVisible(isToolkit || isProvider || isResource);
                        if (isToolkit) {
                            ToolkitNode tkn = (ToolkitNode) node;
                            toggleItem.setText(tkn.getUserObject().isEnabled() ? "Disable Toolkit" : "Enable Toolkit");
                        } else if (isProvider || isResource) {
                            ContextProvider cp = (ContextProvider) ((AbstractContextNode<?>)node).getUserObject();
                            toggleItem.setText(cp.isProviding() ? "Stop Providing" : "Start Providing");
                        }
                        
                        popup.show(e.getComponent(), e.getX(), e.getY());
                    }
                }
            }
        });
    }

    /**
     * Attempts to open the given resource in the host environment's preferred 
     * editor by delegating to the container.
     * 
     * @param res The resource to open.
     */
    private void openResourceInEditor(AbstractResource<?, ?> res) {
        agi.getConfig().getContainer().openResource(res);
    }

    /**
     * Authoritatively applies the standard column widths to the tree table.
     * This method ensures the 'Name' column maintains its 400px width and 
     * token columns are correctly sized.
     * <p>
     * It uses model-to-view conversion to ensure the correct columns are 
     * targeted even if the user has reordered or hidden them.
     * </p>
     */
    private void applyColumnWidths() {
        if (treeTable.getColumnCount() == 0) {
            return;
        }
        
        // 1. Name Column (400px) - Model Index 0
        applyColumnWidth(0, 400, 200);
        
        // 2. Token Columns (Instructions, Declarations, History, RAG) - Model Indices 1-4
        for (int i = 1; i <= 4; i++) {
            applyColumnWidth(i, 80, 50);
        }
        
        // 3. Status Column - Model Index 5
        applyColumnWidth(5, 120, 80);
    }

    /**
     * Helper method to apply width to a specific column identified by its model index.
     * 
     * @param modelIndex The index of the column in the model.
     * @param preferredWidth The desired preferred width.
     * @param minWidth The minimum width (optional, use 0 to skip).
     */
    private void applyColumnWidth(int modelIndex, int preferredWidth, int minWidth) {
        int viewIndex = treeTable.convertColumnIndexToView(modelIndex);
        if (viewIndex != -1) {
            TableColumn col = treeTable.getColumnModel().getColumn(viewIndex);
            col.setPreferredWidth(preferredWidth);
            if (minWidth > 0) {
                col.setMinWidth(minWidth);
            }
        }
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
     * Reloads the panel with the new agi state.
     */
    public void reload() {
        this.agi = agiPanel.getAgi();
        setupListeners();
        refresh(true);
    }

    /**
     * Refreshes the data in the tree table while preserving expansion state 
     * and column widths.
     * 
     * @param structural If true, the underlying model is rebuilt (e.g. on session switch).
     */
    public final void refresh(boolean structural) {
        SwingUtilities.invokeLater(() -> {
            // FIXED: Removed !isShowing() check to ensure model is updated even if tab is hidden.
            // This prevents stale state when the user switches to the Context tab.
            
            log.info("Refreshing ContextPanel tree (structural={}) for agi: {}", structural, agi.getShortId());
            
            // 1. Capture current expansion state
            Set<TreePath> expandedPaths = getExpandedPaths();

            // 2. Update Model
            if (structural) {
                this.treeTableModel = new ContextTreeTableModel(agiPanel);
                treeTable.setTreeTableModel(treeTableModel);
                
                // CRITICAL: Re-apply column settings after model change
                treeTable.setAutoCreateColumnsFromModel(false);
                applyColumnWidths();
            } else {
                // Non-structural refresh: JXTreeTable preserves columns if autoCreate is false
                treeTableModel.refresh();
            }
            
            // 3. Restore state (Nested invokeLater to ensure table has processed model event)
            SwingUtilities.invokeLater(() -> {
                restoreExpandedPaths(expandedPaths);
                
                // Select the first node if nothing is selected
                if (treeTable.getSelectedRow() == -1 && treeTable.getRowCount() > 0) {
                    treeTable.setRowSelectionInterval(0, 0);
                }
                
                treeTable.revalidate();
                treeTable.repaint();
            });
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
        agi.getExecutor().execute(() -> {
            try {
                treeTableModel.refreshTokens();
                SwingUtilities.invokeLater(() -> {
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
        refresh(true);
    }
}
