/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.tool;

import java.util.List;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * A context tree node representing a single message in the conversation.
 *
 * @author anahata
 */
public class MessageNode extends AbstractContextNode<AbstractMessage> {

    /**
     * Constructs a new MessageNode.
     * @param agiPanel The parent agi panel.
     * @param userObject The message to wrap.
     */
    public MessageNode(AgiPanel agiPanel, AbstractMessage userObject) {
        super(agiPanel, userObject);
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
    protected List<?> fetchChildObjects() {
        return userObject.getParts(true);
    }

    /** {@inheritDoc} */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        if (obj instanceof AbstractPart part) {
            return new PartNode(agiPanel, part);
        }
        return null;
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
