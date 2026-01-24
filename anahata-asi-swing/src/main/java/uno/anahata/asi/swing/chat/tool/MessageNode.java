/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractPart;

/**
 * A context tree node representing a single message in the conversation.
 *
 * @author anahata
 */
public class MessageNode extends AbstractContextNode<AbstractMessage> {

    public MessageNode(AbstractMessage userObject) {
        super(userObject);
    }

    @Override
    public String getName() {
        return userObject.getRole() + " #" + userObject.getSequentialId();
    }

    @Override
    public String getDescription() {
        return "Message from " + userObject.getRole() + " at " + userObject.getTimestamp();
    }

    @Override
    public List<AbstractContextNode<?>> getChildren() {
        List<AbstractContextNode<?>> children = new ArrayList<>();
        for (AbstractPart part : userObject.getParts(true)) {
            children.add(new PartNode(part));
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
        return userObject.getTokenCount(false);
    }

    @Override
    public int getRagTokens(Chat chat) {
        return 0;
    }

    @Override
    public String getStatus() {
        return userObject.isPruned() ? "Pruned" : "Active";
    }
}
