/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.tool;

import javax.swing.tree.TreePath;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.treetable.AbstractTreeTableModel;
import uno.anahata.asi.swing.agi.AgiPanel;

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
    /** The parent agi panel. */
    private final AgiPanel agiPanel;

    /**
     * Constructs a new ContextTreeTableModel.
     * @param agiPanel The parent agi panel.
     */
    public ContextTreeTableModel(AgiPanel agiPanel) {
        super(null); 
        this.agiPanel = agiPanel;
        refresh();
    }

    /**
     * Refreshes the model's data from the ContextManager and notifies the view of the change.
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
            log.info("Refreshing existing context tree root for agi: {}", agiPanel.getAgi().getShortId());
            cmn.refresh();
            // fireTreeStructureChanged is the safest way to notify of deep changes.
            // UI stability is maintained by preserving node instances.
            modelSupport.fireTreeStructureChanged(new TreePath(root));
        } else {
            log.info("Creating new context tree root for agi: {}", agiPanel.getAgi().getShortId());
            this.root = new ContextManagerNode(agiPanel, agiPanel.getAgi().getContextManager());
            modelSupport.fireTreeStructureChanged(new TreePath(root));
        }
    }
    
    /**
     * Triggers an explicit recalculation of token counts for all nodes in the tree.
     */
    public void refreshTokens() {
        if (root instanceof AbstractContextNode<?> node) {
            node.refresh();
            modelSupport.fireTreeStructureChanged(new TreePath(root));
        }
    }

    /** {@inheritDoc} */
    @Override
    public int getColumnCount() {
        return 6; // Name, Instructions, Declarations, History, RAG, Status
    }

    /** {@inheritDoc} */
    @Override
    public String getColumnName(int column) {
        return switch (column) {
            case 0 -> "Name";
            case 1 -> "Instructions";
            case 2 -> "Declarations";
            case 3 -> "History";
            case 4 -> "RAG";
            case 5 -> "Status";
            default -> "";
        };
    }

    /** {@inheritDoc} */
    @Override
    public Class<?> getColumnClass(int column) {
        return switch (column) {
            case 1, 2, 3, 4 -> Integer.class;
            default -> String.class;
        };
    }

    /**
     * {@inheritDoc}
     * Implementation details: Delegates value retrieval to the AbstractContextNode 
     * based on the column index.
     */
    @Override
    public Object getValueAt(Object node, int column) {
        if (node instanceof AbstractContextNode<?> cn) {
            return switch (column) {
                case 0 -> cn.getName();
                case 1 -> cn.getInstructionsTokens();
                case 2 -> cn.getDeclarationsTokens();
                case 3 -> cn.getHistoryTokens();
                case 4 -> cn.getRagTokens();
                case 5 -> cn.getStatus();
                default -> null;
            };
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * Implementation details: Delegates to the parent node's getChildren() method.
     */
    @Override
    public Object getChild(Object parent, int index) {
        if (parent instanceof AbstractContextNode<?> cn) {
            return cn.getChildren().get(index);
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * Implementation details: Delegates to the parent node's getChildren().size().
     */
    @Override
    public int getChildCount(Object parent) {
        if (parent instanceof AbstractContextNode<?> cn) {
            return cn.getChildren().size();
        }
        return 0;
    }

    /**
     * {@inheritDoc}
     * Implementation details: Performs a standard indexOf search in the child list.
     */
    @Override
    public int getIndexOfChild(Object parent, Object child) {
        if (parent instanceof AbstractContextNode<?> cn) {
            return cn.getChildren().indexOf(child);
        }
        return -1;
    }

    /**
     * {@inheritDoc}
     * Implementation details: A node is a leaf if its getChildren() list is empty.
     */
    @Override
    public boolean isLeaf(Object node) {
        if (node instanceof AbstractContextNode<?> cn) {
            return cn.getChildren().isEmpty();
        }
        return true;
    }
}
