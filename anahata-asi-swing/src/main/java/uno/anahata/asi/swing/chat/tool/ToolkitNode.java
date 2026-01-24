/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.model.tool.AbstractToolkit;

/**
 * A context tree node representing an {@link AbstractToolkit}.
 * <p>
 * This node acts as a bridge in the hierarchy. It exposes the toolkit's 
 * internal {@link ContextProvider} (the actual tool instance) as a child, 
 * followed by all the individual tools defined within the toolkit.
 * </p>
 *
 * @author anahata
 */
public class ToolkitNode extends AbstractContextNode<AbstractToolkit<?>> {

    /**
     * Constructs a new ToolkitNode.
     * @param userObject The toolkit to wrap.
     */
    public ToolkitNode(AbstractToolkit<?> userObject) {
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
     * 1. It first adds the toolkit's context provider implementation (if any) 
     *    as a child ProviderNode.
     * 2. It then adds all tools in the toolkit as child ToolNodes.
     */
    @Override
    public List<AbstractContextNode<?>> getChildren() {
        List<AbstractContextNode<?>> children = new ArrayList<>();
        
        // 1. The toolkit's context provider implementation (if any)
        ContextProvider cp = userObject.getContextProvider();
        if (cp != null) {
            children.add(new ProviderNode(cp));
        }
        
        // 2. The tools
        for (AbstractTool<?, ?> tool : userObject.getAllTools()) {
            children.add(new ToolNode(tool));
        }
        
        return children;
    }

    /** {@inheritDoc} */
    @Override
    public int getInstructionsTokens(Chat chat) {
        return 0;
    }

    /** {@inheritDoc} */
    @Override
    public int getDeclarationsTokens() {
        return userObject.getTokenCount();
    }

    @Override
    public int getHistoryTokens(Chat chat) {
        return 0;
    }

    /** {@inheritDoc} */
    @Override
    public int getRagTokens(Chat chat) {
        return 0;
    }

    /** {@inheritDoc} */
    @Override
    public String getStatus() {
        return userObject.isEnabled() ? "Enabled" : "Disabled";
    }
}
