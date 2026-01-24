/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.resource.ResourceManager;

/**
 * A context tree node representing the managed resources.
 *
 * @author anahata
 */
public class ResourcesNode extends AbstractContextNode<ResourceManager> {

    public ResourcesNode(ResourceManager userObject) {
        super(userObject);
    }

    @Override
    public String getName() {
        return "Resources";
    }

    @Override
    public String getDescription() {
        return "Managed resources (files, etc.) that can be injected into the context.";
    }

    @Override
    public List<AbstractContextNode<?>> getChildren() {
        List<AbstractContextNode<?>> children = new ArrayList<>();
        for (AbstractResource res : userObject.getResources()) {
            children.add(new ResourceNode(res));
        }
        return children;
    }

    @Override
    public int getInstructionsTokens(Chat chat) {
        return 0;
    }

    @Override
    public int getDeclarationsTokens() {
        return 0;
    }

    @Override
    public int getHistoryTokens(Chat chat) {
        return 0;
    }

    @Override
    public int getRagTokens(Chat chat) {
        return 0;
    }

    @Override
    public String getStatus() {
        return userObject.getResources().size() + " resources";
    }
}
