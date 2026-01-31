/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.swing.chat.ChatPanel;

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
     * @param chatPanel The parent chat panel.
     * @param userObject The message to wrap.
     */
    public MessageNode(ChatPanel chatPanel, AbstractMessage userObject) {
        super(chatPanel, userObject);
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
                children.add(new PartNode(chatPanel, part));
            }
        }
        return children;
    }

    /** {@inheritDoc} */
    @Override
    protected void calculateLocalTokens() {
        // Message tokens are aggregated from PartNodes
    }

    /** {@inheritDoc} */
    @Override
    protected void updateStatus() {
        this.status = userObject.isEffectivelyPruned() ? "Pruned" : "Active";
    }
}
