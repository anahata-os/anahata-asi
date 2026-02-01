/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.tool.AbstractToolkit;
import uno.anahata.asi.swing.chat.ChatPanel;
import uno.anahata.asi.tool.ToolManager;

/**
 * A context tree node representing a {@link ContextProvider}.
 * <p>
 * This node handles the recursive structure of context providers. It has 
 * special logic for the {@link ToolManager}, which exposes its registered 
 * toolkits as child nodes.
 * </p>
 *
 * @author anahata
 */
public class ProviderNode extends AbstractContextNode<ContextProvider> {

    /** The cached list of children. */
    private List<AbstractContextNode<?>> children;

    /**
     * Constructs a new ProviderNode.
     * @param chatPanel The parent chat panel.
     * @param userObject The context provider to wrap.
     */
    public ProviderNode(ChatPanel chatPanel, ContextProvider userObject) {
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
     * 1. If the provider is a ToolManager, it adds all its toolkits as children.
     * 2. For other providers, it adds all standard child providers, filtering 
     *    out those that are already represented as toolkits to avoid duplication.
     */
    @Override
    public List<AbstractContextNode<?>> getChildren() {
        if (children == null) {
            children = new ArrayList<>();
            
            if (userObject instanceof ToolManager tm) {
                // ToolManager is special: it shows toolkits. 
                for (AbstractToolkit<?> tk : tm.getToolkits().values()) {
                    children.add(new ToolkitNode(chatPanel, tk));
                }
            } else {
                // Standard children providers
                for (ContextProvider child : userObject.getChildrenProviders()) {
                    // Avoid double-counting toolkits if they are also providers
                    if (!(child instanceof AbstractToolkit)) {
                        children.add(new ProviderNode(chatPanel, child));
                    }
                }
            }
        }
        return children;
    }

    /** {@inheritDoc} */
    @Override
    protected void calculateLocalTokens() {
        this.instructionsTokens = userObject.getInstructionsTokenCount();
        this.ragTokens = userObject.getRagTokenCount();
    }

    /** {@inheritDoc} */
    @Override
    protected void updateStatus() {
        if (!userObject.isProviding()) {
            this.status = "Disabled";
        } else if (!userObject.isEffectivelyProviding()) {
            this.status = "Disabled (Inherited)";
        } else {
            this.status = "Providing";
        }
    }
}
