/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.Collections;
import java.util.List;
import uno.anahata.asi.model.core.AbstractPart;

/**
 * A context tree node representing a single part of a message.
 *
 * @author anahata
 */
public class PartNode extends AbstractContextNode<AbstractPart> {

    /**
     * Constructs a new PartNode.
     * @param userObject The part to wrap.
     */
    public PartNode(AbstractPart userObject) {
        super(userObject);
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
    public void refreshTokens() {
        this.instructionsTokens = 0;
        this.declarationsTokens = 0;
        this.historyTokens = userObject.getTokenCount();
        this.ragTokens = 0;
        
        if (userObject.isEffectivelyPruned()) {
            this.status = "Pruned (" + userObject.getTurnsLeft() + " turns left)";
        } else {
            this.status = "Active";
        }
    }
}
