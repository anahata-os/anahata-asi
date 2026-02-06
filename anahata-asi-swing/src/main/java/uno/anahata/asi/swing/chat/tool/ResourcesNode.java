/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.resource.ResourceManager;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * A context tree node representing the managed resources.
 *
 * @author anahata
 */
public class ResourcesNode extends AbstractContextNode<ResourceManager> {

    /**
     * Constructs a new ResourcesNode.
     * @param chatPanel The parent chat panel.
     * @param userObject The resource manager to wrap.
     */
    public ResourcesNode(ChatPanel chatPanel, ResourceManager userObject) {
        super(chatPanel, userObject);
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        return "Resources";
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return "Managed resources (files, etc.) that can be injected into the context.";
    }

    /** {@inheritDoc} */
    @Override
    protected List<?> fetchChildObjects() {
        return new ArrayList<>(userObject.getResources());
    }

    /** {@inheritDoc} */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        if (obj instanceof AbstractResource<?, ?> res) {
            return new ResourceNode(chatPanel, res);
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    protected void calculateLocalTokens() {
        // Resources tokens are aggregated from ResourceNodes
    }

    /** {@inheritDoc} */
    @Override
    protected void updateStatus() {
        if (!userObject.isProviding()) {
            this.status = "Disabled";
        } else {
            this.status = "Providing (" + userObject.getResources().size() + " resources)";
        }
    }
}
