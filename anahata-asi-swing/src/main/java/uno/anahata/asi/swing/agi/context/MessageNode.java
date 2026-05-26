/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context;

import java.util.List;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * A context tree node representing a single message in the conversation.
 *
 * @author anahata
 */
public class MessageNode extends AbstractContextNode<AbstractMessage> {

    /**
     * Constructs a new MessageNode.
     *
     * @param agiPanel The parent agi panel.
     * @param userObject The message to wrap.
     */
    public MessageNode(AgiPanel agiPanel, AbstractMessage userObject) {
        super(agiPanel, userObject);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns a composite string containing the role
     * (USER, MODEL) and the sequential identifier from the timeline.
     * </p>
     */
    @Override
    public String getName() {
        return userObject.getRole() + " #" + userObject.getSequentialId();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Provides a human-readable summary of the
     * message's origin and timestamp for the tree sidebar.
     * </p>
     */
    @Override
    public String getDescription() {
        return "Message from " + userObject.getRole() + " at " + userObject.getTimestamp();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Retrieves the list of {@link AbstractPart}s
     * contained in the message, including effectively pruned parts for
     * visualization.
     * </p>
     */
    @Override
    protected List<?> fetchChildObjects() {
        return userObject.getParts(true);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Maps message parts to their navigable
     * {@link PartNode} representation.
     * </p>
     */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        if (obj instanceof AbstractPart part) {
            return new PartNode(agiPanel, part);
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Message-level tokens are calculated as the sum of
     * tokens from all contained parts.
     * </p>
     */
    @Override
    protected void calculateLocalTokens() {
        AbstractModel model = getAgi().getSelectedModel();
        this.historyTokens = (userObject.shouldCreateMetadata() && model != null)
                ? model.countTokens(userObject.createMetadataHeader())
                : 0;
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
     * Implementation details: Updates the status label based on whether the
     * message is pinned, effectively pruned from the context window, or active.
     * </p>
     */
    @Override
    protected void updateStatus() {
        if (userObject.isAllPinned()) {
            this.status = "All Pinned";
        } else if (userObject.isAnyPinned()) {
            this.status = "Some Pinned";
        } else {
            this.status = userObject.isEffectivelyPruned() ? "Pruned" : "Active";
        }
    }
}
