/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import org.apache.commons.lang3.Validate;
import uno.anahata.ai.chat.Chat;

/**
 * The abstract base class for all messages in a conversation, providing common
 * metadata and functionality for the rich, hierarchical V3 domain model. It
 * supports type-safe roles through its subclasses and ensures each message has
 * a unique identity, timestamp, and full access to the chat context.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Setter
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public abstract class AbstractMessage {

    /**
     * A unique, immutable identifier for this message.
     */
    private final String id = UUID.randomUUID().toString();

    /**
     * The timestamp when this message was created, in milliseconds since the
     * epoch.
     */
    private final long timestamp = System.currentTimeMillis();

    /**
     * A monotonically increasing number assigned to the message when it is
     * added to a chat, representing its order in the conversation.
     */
    private long sequentialId;

    /**
     * The number of tokens in this specific message, as reported by the
     * provider. This is crucial for granular cost analysis and context
     * management.
     */
    private int tokenCount;

    /**
     * The list of parts that make up the message content. Made private to
     * enforce encapsulation.
     */
    @Getter(AccessLevel.NONE)
    private final List<AbstractPart> parts = new ArrayList<>();

    /**
     * A backward reference to the Chat session that owns this message. This is
     * the root of the V3 context management system, allowing any domain object
     * to access the full application state. It is intentionally not transient
     * to support full serialization with Kryo.
     */
    private final Chat chat;

    /**
     * A three-state flag for explicit pruning control. - {@code true}: This
     * message is explicitly pruned and will be hidden. - {@code false}: This
     * message is "pinned" and will never be auto-pruned. - {@code null}:
     * (Default) Auto-pruning is active based on the state of its parts.
     */
    private Boolean pruned = null;

    /**
     * Gets the role of the entity that created this message. This is
     * implemented by subclasses to provide compile-time type safety.
     *
     * @return The role of the message creator.
     */
    public abstract Role getRole();

    /**
     * Safely adds a single part to this message, ensuring the bidirectional
     * relationship is correctly established.
     *
     * @param part The part to add.
     */
    
    void addPart(@NonNull AbstractPart part) {
        if (parts.contains(part)) {
            throw new IllegalArgumentException("Part ." + part + " is already a part on this messge:" + this);
        }
        if (part.getMessage() != null) {
            throw new IllegalArgumentException("That part already has a message.");
        }
        //part.setMessage(this);
        this.parts.add(part);
    }
    
    void removePart(AbstractPart part) {
        Validate.isTrue(parts.contains(part), "Not a part of this message " + part);
        parts.remove(part);
    }
    
    /**
     * Safely removes a part from this message, ensuring the bidirectional link
     * is correctly severed.
     *
     * @param part The part to remove.
     */
    /*
    public void removePart(@NonNull AbstractPart part) {
        if (parts.remove(part)) {
            part.setMessage(null); // Break the link
        }
    }
    */

    /**
     * Safely adds a list of parts to this message, ensuring the bidirectional
     * relationship is correctly established for every part.
     *
     * @param parts The list of parts to add.
     */
    /*
    public void addAllParts(@NonNull List<? extends AbstractPart> parts) {
        for (AbstractPart part : parts) {
            addPart(part);
        }
    }
    */

    /**
     * Calculates the "depth" of this message, defined as its distance from the
     * most recent message in the chat history. The head message has a depth of
     * 0.
     *
     * @return The depth of the message, or -1 if not attached to a chat.
     */
    public int getDepth() {
        if (chat == null) {
            return -1;
        }
        // Optimized to fetch the history only once.
        List<AbstractMessage> history = chat.getContextManager().getHistory();
        int index = history.indexOf(this);
        if (index == -1) {//not added to the huistory, no depth
            return -1;
        }
        return history.size() - 1 - index;
    }

    /**
     * Convenience method to get the message content as a single string,
     * concatenating the text representation of all its parts.
     *
     * @param includePrunned wether to include prunned parts
     * @return The concatenated text content.
     */
    public String asText(boolean includePrunned) {
        return getParts(includePrunned).stream()
                .map(AbstractPart::asText)
                .collect(Collectors.joining());
    }

    /**
     * A simple getter for the EXPLICIT pruned state of this message. This is
     * used by child parts to break the circular dependency in pruning logic.
     *
     * @return The explicit pruned state.
     */
    public Boolean isPruned() {
        return pruned;
    }

    /**
     * Calculates the EFFECTIVE pruned state of this message. A message is
     * effectively pruned if it was explicitly pruned OR if all of its
     * constituent parts are effectively pruned.
     *
     * @return {@code true} if the message is effectively pruned.
     */
    public boolean isEffectivelyPruned() {
        // 1. Check the explicit flag on the message itself.
        if (Boolean.TRUE.equals(this.pruned)) {
            return true;
        }
        // 2. If not explicitly pruned, it is effectively pruned only if ALL its parts are.
        // Note: stream().allMatch() correctly returns true for an empty stream.
        return getParts(false).isEmpty() && !parts.isEmpty();
    }

    /**
     * The definitive, encapsulated method for retrieving the parts of a message
     * that should be sent to the model, respecting the pruning policy. This
     * method is designed to be overridden by subclasses like
     * {@link RagMessage}.
     *
     * @param includePruned If true, all parts are returned, bypassing the
     * pruning check.
     * @return A new list of the visible parts.
     */
    public List<AbstractPart> getParts(boolean includePruned) {
        if (includePruned) {
            return getParts();
        } else {
            return parts.stream()
                    .filter(p -> !p.isEffectivelyPruned())
                    .collect(Collectors.toUnmodifiableList());
        }

    }

    public List<AbstractPart> getParts() {
        return Collections.unmodifiableList(parts);
    }
}