/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import javax.swing.tree.TreePath;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.treetable.AbstractTreeTableModel;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextProvider;

/**
 * A TreeTableModel that provides a hierarchical, JNDI-style view of the entire 
 * AI context using unified AbstractContextNodes.
 *
 * @author anahata
 */
@Slf4j
public class ContextTreeTableModel extends AbstractTreeTableModel {
    /** The active chat session. */
    private final Chat chat;
    /** The list of root nodes. */
    private List<AbstractContextNode<?>> rootNodes;
    
    /** A stable, unique object to represent the invisible root. */
    private final Object rootObject = new Object();

    /** The names of the columns in the tree table. */
    private final String[] columnNames = {"Name", "Instructions", "Declarations", "History", "RAG", "Status"};

    /**
     * Constructs a new ContextTreeTableModel.
     * @param chat The active chat session.
     */
    public ContextTreeTableModel(Chat chat) {
        super(new Object()); // Temporary root, will be replaced by rootObject
        this.root = rootObject;
        this.chat = chat;
        this.rootNodes = new ArrayList<>();
        refresh();
    }

    /**
     * Refreshes the model's data from the ContextManager and notifies the view of the change.
     * Implementation details: It rebuilds the root node list from the ContextManager's 
     * top-level providers, history, and resources.
     */
    public final void refresh() {
        log.info("Rebuilding context tree root nodes for chat: {}", chat.getShortId());
        this.rootNodes = new ArrayList<>();
        
        // 1. History
        rootNodes.add(new HistoryNode(chat.getContextManager()));
        
        // 2. Providers
        List<ContextProvider> providers = chat.getContextManager().getProviders();
        log.info("Found {} top-level providers in ContextManager", providers.size());
        for (ContextProvider cp : providers) {
            rootNodes.add(new ProviderNode(cp));
        }
        
        // 3. Resources
        rootNodes.add(new ResourcesNode(chat.getResourceManager()));
        
        log.info("Context tree rebuilt with {} root nodes.", rootNodes.size());
        modelSupport.fireTreeStructureChanged(new TreePath(getRoot()));
    }

    /** {@inheritDoc} */
    @Override
    public int getColumnCount() {
        return columnNames.length;
    }

    /** {@inheritDoc} */
    @Override
    public String getColumnName(int column) {
        return columnNames[column];
    }

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
                case 1 -> cn.getInstructionsTokens(chat);
                case 2 -> cn.getDeclarationsTokens();
                case 3 -> cn.getHistoryTokens(chat);
                case 4 -> cn.getRagTokens(chat);
                case 5 -> cn.getStatus();
                default -> null;
            };
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * Implementation details: Handles the invisible root node by returning from 
     * rootNodes, otherwise delegates to the parent node's getChildren() method.
     */
    @Override
    public Object getChild(Object parent, int index) {
        log.debug("getChild: parent={}, index={}", parent, index);
        if (parent == getRoot()) {
            return rootNodes.get(index);
        } else if (parent instanceof AbstractContextNode<?> cn) {
            return cn.getChildren().get(index);
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * Implementation details: Returns the size of rootNodes for the root, 
     * otherwise delegates to the parent node's getChildren().size().
     */
    @Override
    public int getChildCount(Object parent) {
        log.debug("getChildCount: parent={}", parent);
        if (parent == getRoot()) {
            return rootNodes.size();
        } else if (parent instanceof AbstractContextNode<?> cn) {
            return cn.getChildren().size();
        }
        return 0;
    }

    /**
     * {@inheritDoc}
     * Implementation details: Performs a standard indexOf search in the 
     * appropriate child list.
     */
    @Override
    public int getIndexOfChild(Object parent, Object child) {
        if (parent == getRoot()) {
            return rootNodes.indexOf(child);
        } else if (parent instanceof AbstractContextNode<?> cn) {
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
        if (node == getRoot()) {
            return rootNodes.isEmpty();
        }
        if (node instanceof AbstractContextNode<?> cn) {
            return cn.getChildren().isEmpty();
        }
        return true;
    }
}
