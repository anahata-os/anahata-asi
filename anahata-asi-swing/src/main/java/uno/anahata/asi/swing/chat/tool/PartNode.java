/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.Collections;
import java.util.List;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * A context tree node representing a single part of a message.
 *
 * @author anahata
 */
public class PartNode extends AbstractContextNode<AbstractPart> {

    /**
     * Constructs a new PartNode.
     * @param chatPanel The parent chat panel.
     * @param userObject The part to wrap.
     */
    public PartNode(ChatPanel chatPanel, AbstractPart userObject) {
        super(chatPanel, userObject);
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        return userObject.getClass().getSimpleName() + " #" + userObject.getSequentialId();
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return "A " + userObject.getClass().getSimpleName() + " part.";
    }

    /** {@inheritDoc} */
    @Override
    public List<AbstractContextNode<?>> getChildren() {
        return Collections.emptyList();
    }

    /** {@inheritDoc} */
    @Override
    protected void calculateLocalTokens() {
        this.historyTokens = userObject.getTokenCount();
    }

    /** {@inheritDoc} */
    @Override
    protected void updateStatus() {
        if (userObject.isEffectivelyPruned()) {
            this.status = "Pruned (" + userObject.getTurnsLeft() + " turns left)";
        } else {
            this.status = "Active";
        }
    }
}
