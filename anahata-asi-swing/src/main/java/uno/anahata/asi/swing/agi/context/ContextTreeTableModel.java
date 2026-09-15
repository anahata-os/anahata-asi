/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context;

import javax.swing.tree.TreePath;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.treetable.AbstractTreeTableModel;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.resources.ResourcesNode;
import uno.anahata.asi.swing.internal.SwingTask;

/**
 * A TreeTableModel that provides a hierarchical, JNDI-style view of the entire
 * AI context using unified AbstractContextNodes.
 * <p>
 * This model is designed for high performance and UI stability. It preserves
 * node identity across refreshes, ensuring that the tree view remains stable
 * (no jumping or collapsing) when the underlying context changes.
 * </p>
 * <p>
 * It uses a single {@link ContextManagerNode} as the root of the hierarchy.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ContextTreeTableModel extends AbstractTreeTableModel {

    /**
     * The parent agi panel.
     */
    private final AgiPanel agiPanel;

    /**
     * Constructs a new ContextTreeTableModel.
     *
     * @param agiPanel The parent agi panel.
     */
    public ContextTreeTableModel(AgiPanel agiPanel) {
        super(null);
        this.agiPanel = agiPanel;
        refresh();
    }

    /**
     * Refreshes the model's data from the ContextManager and notifies the view
     * of the change.
     * <p>
     * Implementation details: It preserves the root node instance if it already
     * exists, triggering a recursive refresh of the node hierarchy.
     * </p>
     * <p>
     * To prevent the 'jumping' behavior, it uses identity-preserving nodes and
     * fires a structure change event on the root.
     * </p>
     */
    public final void refresh() {
        if (this.root instanceof ContextManagerNode cmn) {
            log.debug("Refreshing existing context tree root for agi: {}", agiPanel.getAgi().getShortId());
            cmn.refresh();
            // fireTreeStructureChanged is the safest way to notify of deep changes.
            // UI stability is maintained by preserving node instances.
            modelSupport.fireTreeStructureChanged(new TreePath(root));
        } else {
            log.debug("Creating new context tree root for agi: {}", agiPanel.getAgi().getShortId());
            this.root = new ContextManagerNode(agiPanel, agiPanel.getAgi().getContextManager());
            modelSupport.fireTreeStructureChanged(new TreePath(root));
        }
    }

    /**
     * Triggers an asynchronous recalculation of token counts for all nodes in
     * the tree. Uses SwingTask to run the calculation pass on a background
     * thread.
     * <p>
     * Implementation details: Emits {@code firePathChanged} instead of resetting
     * the tree structure, ensuring zero flickering and preserving active row selection.
     * </p>
     *
     * @param onDone An optional callback to run on the EDT after the refresh is
     * complete.
     */
    public void refreshTokens(Runnable onDone) {
        if (root instanceof AbstractContextNode<?> node) {
            new SwingTask<Void>(agiPanel, "Calculating Tokens", () -> {
                node.refreshData();
                return null;
            }, (v) -> {
                modelSupport.firePathChanged(new TreePath(root));
                if (onDone != null) {
                    onDone.run();
                }
            }, null, false).start();
        }
    }

    /**
     * Refreshes the child structure of a specific branch node without resetting the rest of the tree.
     *
     * @param branch The branch node whose children changed.
     */
    public void refreshBranch(AbstractContextNode<?> branch) {
        if (branch != null) {
            branch.refresh();
            modelSupport.fireTreeStructureChanged(branch.getTreePath());
        }
    }

    /**
     * Notifies the tree table that a specific node's data (tokens, status) has changed.
     *
     * @param node The node whose data changed.
     */
    public void refreshNodeData(AbstractContextNode<?> node) {
        if (node != null) {
            node.refreshData();
            modelSupport.firePathChanged(node.getTreePath());
        }
    }

    /**
     * Gets the child node representing the conversation history.
     *
     * @return The HistoryNode, or null if not yet created.
     */
    public HistoryNode getHistoryNode() {
        if (root instanceof ContextManagerNode cmn) {
            return cmn.getHistoryNode();
        }
        return null;
    }

    /**
     * Gets the child node representing the managed resources.
     *
     * @return The ResourcesNode, or null if not yet created.
     */
    public ResourcesNode getResourcesNode() {
        if (root instanceof ContextManagerNode cmn) {
            return cmn.getResourcesNode();
        }
        return null;
    }

    /**
     * Recursively searches for a node wrapping the specified domain user object.
     *
     * @param userObject The domain user object to search for.
     * @return The matching AbstractContextNode, or null if not found.
     */
    public AbstractContextNode<?> findNode(Object userObject) {
        if (root instanceof AbstractContextNode<?> rootNode) {
            return findNodeRecursive(rootNode, userObject);
        }
        return null;
    }

    private AbstractContextNode<?> findNodeRecursive(AbstractContextNode<?> current, Object userObject) {
        if (current.getUserObject() == userObject) {
            return current;
        }
        for (AbstractContextNode<?> child : current.getChildren()) {
            AbstractContextNode<?> found = findNodeRecursive(child, userObject);
            if (found != null) {
                return found;
            }
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getColumnCount() {
        return 7; // Name, Total, Instructions, Declarations, History, RAG, Status
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getColumnName(int column) {
        return switch (column) {
            case 0 ->
                "Name";
            case 1 ->
                "Total";
            case 2 ->
                "Instructions";
            case 3 ->
                "Declarations";
            case 4 ->
                "History";
            case 5 ->
                "RAG";
            case 6 ->
                "Status";
            default ->
                "";
        };
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<?> getColumnClass(int column) {
        return switch (column) {
            case 1, 2, 3, 4, 5 ->
                Integer.class;
            default ->
                String.class;
        };
    }

    /**
     * {@inheritDoc} Implementation details: Delegates value retrieval to the
     * AbstractContextNode based on the column index.
     */
    @Override
    public Object getValueAt(Object node, int column) {
        if (node instanceof AbstractContextNode<?> cn) {
            return switch (column) {
                case 0 ->
                    cn.getName();
                case 1 ->
                    cn.getInstructionsTokens() + cn.getDeclarationsTokens() + cn.getHistoryTokens() + cn.getRagTokens();
                case 2 ->
                    cn.getInstructionsTokens();
                case 3 ->
                    cn.getDeclarationsTokens();
                case 4 ->
                    cn.getHistoryTokens();
                case 5 ->
                    cn.getRagTokens();
                case 6 ->
                    cn.getStatus();
                default ->
                    null;
            };
        }
        return null;
    }

    /**
     * {@inheritDoc} Implementation details: Delegates to the parent node's
     * getChildren() method.
     */
    @Override
    public Object getChild(Object parent, int index) {
        if (parent instanceof AbstractContextNode<?> cn) {
            return cn.getChildren().get(index);
        }
        return null;
    }

    /**
     * {@inheritDoc} Implementation details: Delegates to the parent node's
     * getChildren().size().
     */
    @Override
    public int getChildCount(Object parent) {
        if (parent instanceof AbstractContextNode<?> cn) {
            return cn.getChildren().size();
        }
        return 0;
    }

    /**
     * {@inheritDoc} Implementation details: Performs a standard indexOf search
     * in the child list.
     */
    @Override
    public int getIndexOfChild(Object parent, Object child) {
        if (parent instanceof AbstractContextNode<?> cn) {
            return cn.getChildren().indexOf(child);
        }
        return -1;
    }

    /**
     * {@inheritDoc} Implementation details: A node is a leaf if its
     * getChildren() list is empty.
     */
    @Override
    public boolean isLeaf(Object node) {
        if (node instanceof AbstractContextNode<?> cn) {
            return cn.getChildren().isEmpty();
        }
        return true;
    }
}
