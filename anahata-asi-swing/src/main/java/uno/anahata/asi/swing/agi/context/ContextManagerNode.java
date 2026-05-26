/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context;

import uno.anahata.asi.swing.agi.resources.ResourcesNode;
import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.agi.context.ContextManager;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.agi.tool.spi.AbstractToolkit;
import uno.anahata.asi.agi.resource.ResourceManager;
import uno.anahata.asi.agi.tool.ToolManager;
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
     *
     * @param agiPanel The parent agi panel.
     * @param userObject The context manager to wrap.
     */
    public ContextManagerNode(AgiPanel agiPanel, ContextManager userObject) {
        super(agiPanel, userObject);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns the static identifier "Context Manager",
     * marking this node as the root of the AGI's environmental awareness.
     * </p>
     */
    @Override
    public String getName() {
        return "Context Manager";
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Provides a high-level overview of the root
     * orchestrator's role in maintaining the conversation and provider state.
     * </p>
     */
    @Override
    public String getDescription() {
        return "The central orchestrator for the AI context, managing providers, resources, and history.";
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Aggregates all registered
     * {@link ContextProvider}s (excluding toolkits) and injects the manager
     * itself to represent the conversation history branch.
     * </p>
     */
    @Override
    protected List<?> fetchChildObjects() {
        List<Object> objects = new ArrayList<>();

        // 1. Providers (excluding toolkits which are handled by ToolManager)
        for (ContextProvider cp : userObject.getProviders()) {
            if (cp instanceof AbstractToolkit) {
                continue;
            }
            objects.add(cp);
        }

        // 2. History (The ContextManager itself acts as the domain object for history)
        objects.add(userObject);

        return objects;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Maps domain-level orchestrators to their
     * specialized context nodes, including {@link ResourcesNode} and
     * {@link HistoryNode}.
     * </p>
     */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        if (obj instanceof ResourceManager rm2) {
            return new ResourcesNode(agiPanel, rm2);
        } else if (obj instanceof ToolManager tm) {
            return new ToolManagerNode(agiPanel, tm);
        } else if (obj instanceof ContextProvider cp) {
            return new ContextProviderNode(agiPanel, cp);
        } else if (obj instanceof ContextManager cm) {
            return new HistoryNode(agiPanel, cm);
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: The manager node is a logical container and does
     * not contribute tokens directly. All token metrics are aggregated from its
     * children.
     * </p>
     */
    @Override
    protected void calculateLocalTokens() {
        // The manager itself doesn't have tokens, it just aggregates.
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isActive() {
        return true;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Fixed to "Providing" as the manager is always the
     * active source of the AGI's context.
     * </p>
     */
    @Override
    protected void updateStatus() {
        this.status = "Providing";
    }
}
