/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.tool.AbstractToolkit;
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

    /**
     * Constructs a new ProviderNode.
     * @param userObject The context provider to wrap.
     */
    public ProviderNode(ContextProvider userObject) {
        super(userObject);
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
     * 2. It then adds all standard child providers, filtering out those that 
     *    are already represented as toolkits to avoid duplication.
     */
    @Override
    public List<AbstractContextNode<?>> getChildren() {
        List<AbstractContextNode<?>> children = new ArrayList<>();
        
        if (userObject instanceof ToolManager tm) {
            // ToolManager is special: it shows toolkits
            for (AbstractToolkit<?> tk : tm.getToolkits().values()) {
                children.add(new ToolkitNode(tk));
            }
        }
        
        // Standard children providers
        for (ContextProvider child : userObject.getChildrenProviders()) {
            // Avoid double-counting toolkits if they are also providers
            if (!(child instanceof AbstractToolkit)) {
                children.add(new ProviderNode(child));
            }
        }
        return children;
    }

    /** {@inheritDoc} */
    @Override
    public int getInstructionsTokens(Chat chat) {
        return userObject.getInstructionsTokenCount(chat);
    }

    /** {@inheritDoc} */
    @Override
    public int getDeclarationsTokens() {
        return 0;
    }

    @Override
    public int getHistoryTokens(Chat chat) {
        return 0;
    }

    /** {@inheritDoc} */
    @Override
    public int getRagTokens(Chat chat) {
        return userObject.getRagTokenCount(chat);
    }

    /** {@inheritDoc} */
    @Override
    public String getStatus() {
        return userObject.isProviding() ? "Active" : "Inactive";
    }
}
