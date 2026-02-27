/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.tool;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.context.ContextManager;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.tool.AbstractToolkit;
import uno.anahata.asi.resource.ResourceManager;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * The root node of the context tree, representing the {@link ContextManager}.
 * It aggregates all top-level providers and the conversation history.
 *
 * @author anahata
 */
public class ContextManagerNode extends AbstractContextNode<ContextManager> {

    /**
     * Constructs a new ContextManagerNode.
     * @param agiPanel The parent agi panel.
     * @param userObject The context manager to wrap.
     */
    public ContextManagerNode(AgiPanel agiPanel, ContextManager userObject) {
        super(agiPanel, userObject);
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
    protected List<?> fetchChildObjects() {
        List<Object> objects = new ArrayList<>();
        
        // 1. Providers (excluding toolkits which are handled by ToolManager)
        for (ContextProvider cp : userObject.getProviders()) {
            if (cp instanceof AbstractToolkit) continue;
            objects.add(cp);
        }
        
        // 2. History (The ContextManager itself acts as the domain object for history)
        objects.add(userObject);
        
        return objects;
    }

    /** {@inheritDoc} */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        if (obj instanceof ResourceManager rm) {
            return new ResourcesNode(agiPanel, rm);
        } else if (obj instanceof ContextProvider cp) {
            return new ProviderNode(agiPanel, cp);
        } else if (obj instanceof ContextManager cm) {
            return new HistoryNode(agiPanel, cm);
        }
        return null;
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
