/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.tool.AbstractToolkit;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * A context tree node representing an {@link AbstractToolkit}.
 * <p>
 * This node provides a simplified hierarchy with two primary children:
 * 1. "Context": The toolkit's internal context provider (if applicable).
 * 2. "Tools": A container node for all individual tools.
 * </p>
 *
 * @author anahata
 */
public class ToolkitNode extends AbstractContextNode<AbstractToolkit<?>> {

    /** The cached list of children. */
    private List<AbstractContextNode<?>> children;

    /**
     * Constructs a new ToolkitNode.
     * @param chatPanel The parent chat panel.
     * @param userObject The toolkit to wrap.
     */
    public ToolkitNode(ChatPanel chatPanel, AbstractToolkit<?> userObject) {
        super(chatPanel, userObject);
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        return userObject.getName();
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return userObject.getDescription();
    }

    /**
     * {@inheritDoc}
     * Implementation details:
     * 1. If the toolkit has a context provider, it adds it as a child 
     *    ProviderNode with the display name "Context".
     * 2. It then adds a single ToolsNode container for all tools.
     */
    @Override
    public List<AbstractContextNode<?>> getChildren() {
        if (children == null) {
            children = new ArrayList<>();
            
            // 1. The toolkit's context provider implementation (if any)
            ContextProvider cp = userObject.getContextProvider();
            if (cp != null) {
                children.add(new ProviderNode(chatPanel, cp) {
                    @Override
                    public String getName() {
                        return "Context";
                    }
                });
            }
            
            // 2. The tools container
            children.add(new ToolsNode(chatPanel, userObject));
        }
        return children;
    }

    /** {@inheritDoc} */
    @Override
    protected void calculateLocalTokens() {
        // Toolkit tokens are aggregated from ToolsNode and ProviderNode
    }

    /** {@inheritDoc} */
    @Override
    protected void updateStatus() {
        this.status = userObject.isEnabled() ? "Enabled" : "Disabled";
    }
}
