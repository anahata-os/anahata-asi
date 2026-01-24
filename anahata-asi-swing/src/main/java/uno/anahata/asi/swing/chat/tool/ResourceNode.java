/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.Collections;
import java.util.List;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.model.resource.AbstractResource;

/**
 * A context tree node representing a single managed resource.
 *
 * @author anahata
 */
public class ResourceNode extends AbstractContextNode<AbstractResource> {

    public ResourceNode(AbstractResource userObject) {
        super(userObject);
    }

    @Override
    public String getName() {
        return userObject.getName();
    }

    @Override
    public String getDescription() {
        return userObject.getDescription();
    }

    @Override
    public List<AbstractContextNode<?>> getChildren() {
        return Collections.emptyList();
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
        return userObject.getContextPosition().name() + " | " + userObject.getRefreshPolicy().name();
    }
}
