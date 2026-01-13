/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.tool;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import javax.swing.tree.TreePath;
import org.jdesktop.swingx.treetable.AbstractTreeTableModel;
import uno.anahata.ai.model.tool.AbstractTool;
import uno.anahata.ai.model.tool.AbstractToolkit;
import uno.anahata.ai.tool.ToolManager;

/**
 * A TreeTableModel that provides a hierarchical view of Toolkits and their Tools,
 * including a calculated token count for each tool's schema.
 *
 * @author anahata
 */
public class ToolkitTreeTableModel extends AbstractTreeTableModel {
    private final ToolManager toolManager;
    private List<AbstractToolkit<?>> toolkits;

    private final String[] columnNames = {"Tool / Toolkit", "Permission", "Tokens", "Description"};

    public ToolkitTreeTableModel(ToolManager toolManager) {
        super(new Object()); // The invisible root node
        this.toolManager = toolManager;
        refresh();
    }

    /**
     * Refreshes the model's data from the ToolManager and notifies the view of the change.
     */
    public final void refresh() {
        this.toolkits = new ArrayList<>(toolManager.getToolkits().values());
        this.toolkits.sort(Comparator.comparing(AbstractToolkit::getName));
        // Fire a change from the root to signal that the children have been reloaded.
        modelSupport.fireTreeStructureChanged(new TreePath(getRoot()));
    }

    @Override
    public int getColumnCount() {
        return columnNames.length;
    }

    @Override
    public String getColumnName(int column) {
        return columnNames[column];
    }

    @Override
    public Object getValueAt(Object node, int column) {
        if (node instanceof AbstractToolkit) {
            AbstractToolkit<?> toolkit = (AbstractToolkit<?>) node;
            switch (column) {
                case 0: return toolkit.getName();
                case 1: return toolkit.isEnabled() ? "Enabled" : "Disabled";
                case 2: return toolkit.getTokenCount(); // Use new core model method
                case 3: return toolkit.getDescription();
            }
        } else if (node instanceof AbstractTool) {
            AbstractTool<?, ?> tool = (AbstractTool<?, ?>) node;
            switch (column) {
                case 0: return tool.getName();
                case 1: return tool.getPermission().name();
                case 2: return tool.getTokenCount(); // Use new core model method
                case 3: return tool.getDescription();
            }
        }
        return null;
    }

    @Override
    public Object getChild(Object parent, int index) {
        if (parent == getRoot()) {
            return toolkits.get(index);
        } else if (parent instanceof AbstractToolkit) {
            return ((AbstractToolkit<?>) parent).getAllTools().get(index);
        }
        return null;
    }

    @Override
    public int getChildCount(Object parent) {
        if (parent == getRoot()) {
            return toolkits.size();
        } else if (parent instanceof AbstractToolkit) {
            return ((AbstractToolkit<?>) parent).getAllTools().size();
        }
        return 0;
    }

    @Override
    public int getIndexOfChild(Object parent, Object child) {
        if (parent == getRoot()) {
            return toolkits.indexOf(child);
        } else if (parent instanceof AbstractToolkit) {
            return ((AbstractToolkit<?>) parent).getAllTools().indexOf(child);
        }
        return -1;
    }

    @Override
    public boolean isLeaf(Object node) {
        return node instanceof AbstractTool;
    }
}
