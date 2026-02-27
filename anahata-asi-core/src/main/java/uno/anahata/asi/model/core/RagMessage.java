/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.agi.Agi;

/**
 * A specialized UserMessage that represents just-in-time, ephemeral context
 * provided by the environment (Retrieval-Augmented Generation).
 * <p>
 * This message and its parts are not part of the persistent conversation history
 * and are exempt from all pruning logic.
 * </p>
 *
 * @author Anahata
 */
public class RagMessage extends UserMessage {

    /**
     * Constructs a new RagMessage.
     * @param agi The parent agi session.
     */
    public RagMessage(Agi agi) {
        super(agi);
    }
    
    /**
     * Overrides the default pruning logic to declare that this message is
     * ephemeral and should never be pruned. This is the key to preventing
     * the pruning system from trying to access the (null) Agi context
     * of this non-history message.
     *
     * @return Always {@code false}.
     */
    @Override
    public boolean isEffectivelyPruned() {
        return false;
    }
    
    /**
     * Overrides the default part filtering logic. Since a RagMessage is always
     * ephemeral and relevant for the current turn, it always returns all of its
     * parts, regardless of the pruning flag.
     *
     * @param includePruned This parameter is ignored.
     * @return A new list containing all parts of this message.
     */
    @Override
    public List<AbstractPart> getParts(boolean includePruned) {
        return getParts();
    }

    /** {@inheritDoc} */
    @Override
    public String getFrom() {
        return "Java workspace";
    }

    /** {@inheritDoc} */
    @Override
    public boolean shouldCreateMetadata() {
        return false;
    }
}
