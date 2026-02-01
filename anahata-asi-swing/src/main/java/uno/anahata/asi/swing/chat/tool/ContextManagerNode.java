/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.context.ContextManager;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.tool.AbstractToolkit;
import uno.anahata.asi.resource.ResourceManager;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * The root node of the context tree, representing the {@link ContextManager}.
 * It aggregates all top-level providers and the conversation history.
 *
 * @author anahata
 */
public class ContextManagerNode extends AbstractContextNode<ContextManager> {

    /** The cached list of children. */
    private List<AbstractContextNode<?>> children;

    /**
     * Constructs a new ContextManagerNode.
     * @param chatPanel The parent chat panel.
     * @param userObject The context manager to wrap.
     */
    public ContextManagerNode(ChatPanel chatPanel, ContextManager userObject) {
        super(chatPanel, userObject);
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        return "Context Manager";
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return "The central orchestrator for the AI context, managing providers, resources, and history.";
    }

    /** {@inheritDoc} */
    @Override
    public List<AbstractContextNode<?>> getChildren() {
        if (children == null) {
            children = new ArrayList<>();
            
            // 1. Providers (excluding toolkits which are handled by ToolManager)
            for (ContextProvider cp : userObject.getProviders()) {
                if (cp instanceof AbstractToolkit) continue;
                
                if (cp instanceof ResourceManager rm) {
                    children.add(new ResourcesNode(chatPanel, rm));
                } else {
                    children.add(new ProviderNode(chatPanel, cp));
                }
            }
            
            // 2. History
            children.add(new HistoryNode(chatPanel, userObject));
        }
        return children;
    }

    /** {@inheritDoc} */
    @Override
    protected void calculateLocalTokens() {
        // The manager itself doesn't have tokens, it just aggregates.
    }

    /** {@inheritDoc} */
    @Override
    protected void updateStatus() {
        this.status = "Providing";
    }
}
