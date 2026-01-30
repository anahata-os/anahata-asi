/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractPart;

/**
 * A context tree node representing a single message in the conversation.
 *
 * @author anahata
 */
public class MessageNode extends AbstractContextNode<AbstractMessage> {

    /** The cached list of children. */
    private List<AbstractContextNode<?>> children;

    /**
     * Constructs a new MessageNode.
     * @param userObject The message to wrap.
     */
    public MessageNode(AbstractMessage userObject) {
        super(userObject);
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        return userObject.getRole() + " #" + userObject.getSequentialId();
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return "Message from " + userObject.getRole() + " at " + userObject.getTimestamp();
    }

    /** {@inheritDoc} */
    @Override
    public List<AbstractContextNode<?>> getChildren() {
        if (children == null) {
            children = new ArrayList<>();
            for (AbstractPart part : userObject.getParts(true)) {
                children.add(new PartNode(part));
            }
        }
        return children;
    }

    /** {@inheritDoc} */
    @Override
    public void refreshTokens() {
        this.instructionsTokens = 0;
        this.declarationsTokens = 0;
        this.historyTokens = userObject.getTokenCount(false);
        this.ragTokens = 0;
        this.status = userObject.isEffectivelyPruned() ? "Pruned" : "Active";
        
        for (AbstractContextNode<?> child : getChildren()) {
            child.refreshTokens();
        }
    }
}
