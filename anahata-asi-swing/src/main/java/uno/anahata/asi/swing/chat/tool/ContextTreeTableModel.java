/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import javax.swing.tree.TreePath;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.treetable.AbstractTreeTableModel;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * A TreeTableModel that provides a hierarchical, JNDI-style view of the entire 
 * AI context using unified AbstractContextNodes.
 * <p>
 * This model is designed for high performance by using cached token counts 
 * and status fields in the nodes, ensuring that the {@code getValueAt} 
 * method remains O(1).
 * </p>
 * <p>
 * It uses a single {@link ContextManagerNode} as the root of the hierarchy.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ContextTreeTableModel extends AbstractTreeTableModel {
    /** The parent chat panel. */
    private final ChatPanel chatPanel;

    /**
     * Constructs a new ContextTreeTableModel.
     * @param chatPanel The parent chat panel.
     */
    public ContextTreeTableModel(ChatPanel chatPanel) {
        super(null); 
        this.chatPanel = chatPanel;
        refresh();
    }

    /**
     * Refreshes the model's data from the ContextManager and notifies the view of the change.
     * Implementation details: It rebuilds the root node as a single ContextManagerNode.
     */
    public final void refresh() {
        log.info("Rebuilding context tree root for chat: {}", chatPanel.getChat().getShortId());
        this.root = new ContextManagerNode(chatPanel, chatPanel.getChat().getContextManager());
        modelSupport.fireTreeStructureChanged(new TreePath(root));
    }
    
    /**
     * Triggers an explicit recalculation of token counts for all nodes in the tree.
     */
    public void refreshTokens() {
        if (root instanceof AbstractContextNode<?> node) {
            node.refreshTokens();
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
