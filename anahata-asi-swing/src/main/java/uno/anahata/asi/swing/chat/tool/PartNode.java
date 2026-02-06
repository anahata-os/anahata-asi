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
    protected List<?> fetchChildObjects() {
        return Collections.emptyList();
    }

    /** {@inheritDoc} */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    protected void calculateLocalTokens() {
        this.historyTokens = userObject.getTokenCount();
    }

    /** {@inheritDoc} */
    @Override
    protected void updateStatus() {
        int turnsLeft = userObject.getTurnsLeft();
        if (userObject.isEffectivelyPruned()) {
            this.status = "Pruned" + (turnsLeft >= 0 ? " (" + turnsLeft + " turns left)" : "");
        } else {
            this.status = "Active" + (turnsLeft >= 0 ? " (" + turnsLeft + " turns left)" : "");
        }
    }
}
