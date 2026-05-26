/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context;

import java.util.Collections;
import java.util.List;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * A context tree node representing a single part of a message.
 *
 * @author anahata
 */
public class PartNode extends AbstractContextNode<AbstractPart> {

    /**
     * Constructs a new PartNode.
     *
     * @param agiPanel The parent agi panel.
     * @param userObject The part to wrap.
     */
    public PartNode(AgiPanel agiPanel, AbstractPart userObject) {
        super(agiPanel, userObject);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns the simple class name of the part (e.g.,
     * TextPart, BlobPart) along with its sequential ID.
     * </p>
     */
    @Override
    public String getName() {
        return userObject.getClass().getSimpleName() + " #" + userObject.getSequentialId();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns a basic description including the
     * concrete type of the message part.
     * </p>
     */
    @Override
    public String getDescription() {
        return "A " + userObject.getClass().getSimpleName() + " part.";
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Parts are leaf nodes and do not have child
     * objects.
     * </p>
     */
    @Override
    protected List<?> fetchChildObjects() {
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        return null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Retrieves the token count directly from the
     * underlying domain part for history metrics.
     * </p>
     */
    @Override
    protected void calculateLocalTokens() {
        this.historyTokens = userObject.isEffectivelyPruned() ? userObject.getMetadataTokenCount() : userObject.getEffectiveTokenCount();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isActive() {
        return !userObject.isEffectivelyPruned();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Includes the remaining depth metric in the status
     * string to visualize the Context Window Garbage Collection (CwGC) state.
     * </p>
     */
    @Override
    protected void updateStatus() {
        int remainingDepth = userObject.getRemainingDepth();
        String suffix = "turns left";
        if (remainingDepth < 0) {
            remainingDepth = remainingDepth * -1;
            suffix = "turns ago";
        }
        if (userObject.isPinned()) {
            this.status = "Pinned";
        } else if (userObject.isEffectivelyPruned()) {
            this.status = "Pruned (" + remainingDepth + " " + suffix + ")";
        } else {
            this.status = "Auto (" + remainingDepth + " " + suffix + ")";
        }
    }
}
