/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.resource.ResourceManager;

/**
 * A context tree node representing the managed resources.
 *
 * @author anahata
 */
public class ResourcesNode extends AbstractContextNode<ResourceManager> {

    /** The cached list of children. */
    private List<AbstractContextNode<?>> children;

    /**
     * Constructs a new ResourcesNode.
     * @param userObject The resource manager to wrap.
     */
    public ResourcesNode(ResourceManager userObject) {
        super(userObject);
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
    public List<AbstractContextNode<?>> getChildren() {
        if (children == null) {
            children = new ArrayList<>();
            for (AbstractResource res : userObject.getResources()) {
                children.add(new ResourceNode(res));
            }
        }
        return children;
    }

    /** {@inheritDoc} */
    @Override
    public void refreshTokens() {
        this.instructionsTokens = 0;
        this.declarationsTokens = 0;
        this.historyTokens = 0;
        this.ragTokens = 0;
        this.status = userObject.getResources().size() + " resources";
        
        for (AbstractContextNode<?> child : getChildren()) {
            child.refreshTokens();
        }
    }
}
