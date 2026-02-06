/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

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

    /** {@inheritDoc} */
    @Override
    protected List<?> fetchChildObjects() {
        return userObject.getAllTools();
    }

    /** {@inheritDoc} */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        if (obj instanceof AbstractTool<?, ?> tool) {
            return new ToolNode(chatPanel, tool);
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    protected void calculateLocalTokens() {
        // Tools tokens are aggregated from ToolNodes
    }

    /** {@inheritDoc} */
    @Override
    protected void updateStatus() {
        if (!userObject.isEnabled()) {
            this.status = "Disabled";
        } else if (!userObject.getToolManager().isEffectivelyProviding()) {
            this.status = "Disabled (Inherited)";
        } else {
            this.status = userObject.getAllTools().size() + " tools";
        }
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
