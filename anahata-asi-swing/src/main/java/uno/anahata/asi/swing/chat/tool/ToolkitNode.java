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

    /** {@inheritDoc} */
    @Override
    protected List<?> fetchChildObjects() {
        List<Object> objects = new ArrayList<>();
        
        // 1. The toolkit's context provider implementation (if any)
        ContextProvider cp = userObject.getContextProvider();
        if (cp != null) {
            objects.add(cp);
        }
        
        // 2. The tools container (The toolkit itself acts as the domain object for tools)
        objects.add(userObject);
        
        return objects;
    }

    /** {@inheritDoc} */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        if (obj instanceof ContextProvider cp) {
            return new ProviderNode(chatPanel, cp) {
                @Override
                public String getName() {
                    return "Context";
                }
            };
        } else if (obj instanceof AbstractToolkit<?> tk) {
            return new ToolsNode(chatPanel, tk);
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    protected void calculateLocalTokens() {
        // Toolkit tokens are aggregated from ToolsNode and ProviderNode
    }

    /** {@inheritDoc} */
    @Override
    protected void updateStatus() {
        if (!userObject.isEnabled()) {
            this.status = "Disabled";
        } else if (!userObject.getToolManager().isEffectivelyProviding()) {
            this.status = "Disabled (Inherited)";
        } else {
            this.status = "Enabled";
        }
    }
}
