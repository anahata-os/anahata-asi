/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import javax.swing.Icon;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.model.tool.AbstractToolkit;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * A context tree node that acts as a container for all tools within a toolkit.
 *
 * @author anahata
 */
public class ToolsNode extends AbstractContextNode<AbstractToolkit<?>> {

    /** The cached list of children. */
    private List<AbstractContextNode<?>> children;

    /**
     * Constructs a new ToolsNode.
     * @param chatPanel The parent chat panel.
     * @param userObject The parent toolkit.
     */
    public ToolsNode(ChatPanel chatPanel, AbstractToolkit<?> userObject) {
        super(chatPanel, userObject);
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        return "Tools";
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return "Available tools provided by the " + userObject.getName() + " toolkit.";
    }

    /**
     * {@inheritDoc}
     * Implementation details: Returns a list of ToolNodes for all tools in the toolkit.
     */
    @Override
    public List<AbstractContextNode<?>> getChildren() {
        if (children == null) {
            children = new ArrayList<>();
            for (AbstractTool<?, ?> tool : userObject.getAllTools()) {
                children.add(new ToolNode(chatPanel, tool));
            }
        }
        return children;
    }

    /** {@inheritDoc} */
    @Override
    protected void calculateLocalTokens() {
        // Tools tokens are aggregated from ToolNodes
    }

    /** {@inheritDoc} */
    @Override
    protected void updateStatus() {
        this.status = userObject.getAllTools().size() + " tools";
    }

    /**
     * {@inheritDoc}
     * Overridden to return null, signaling the renderer to use the specialized 
     * 'Tools' container icon instead of the generic toolkit icon.
     */
    @Override
    public Icon getIcon() {
        return null;
    }
}
