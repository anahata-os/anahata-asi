/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.core;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.Setter;
import uno.anahata.ai.Chat;

/**
 * The abstract base class for all messages in a conversation, providing common
 * metadata and functionality for the rich, hierarchical V3 domain model.
 * It supports type-safe roles through its subclasses and ensures each message
 * has a unique identity, timestamp, and full access to the chat context.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Setter
public abstract class AbstractMessage {
    /**
     * A unique, immutable identifier for this message.
     */
    private final String id = UUID.randomUUID().toString();

    /**
     * The timestamp when this message was created, in milliseconds since the epoch.
     */
    private final long timestamp = System.currentTimeMillis();
    
    /**
     * A monotonically increasing number assigned to the message when it is added to a chat,
     * representing its order in the conversation.
     */
    private long sequenceNumber;
    
    /**
     * The number of tokens in this specific message, as reported by the provider.
     * This is crucial for granular cost analysis and context management.
     */
    private int tokenCount;

    /**
     * The list of parts that make up the message content.
     */
    private List<AbstractPart> parts = new ArrayList<>();

    /**
     * A backward reference to the Chat session that owns this message.
     * This is the root of the V3 context management system, allowing any domain
     * object to access the full application state. It is intentionally not
     * transient to support full serialization with Kryo.
     */
    private Chat chat;
    
    /**
     * A three-state flag for explicit pruning control.
     * - {@code true}: This message is explicitly pruned and will be hidden.
     * - {@code false}: This message is "pinned" and will never be auto-pruned.
     * - {@code null}: (Default) Auto-pruning is active based on the state of its parts.
     */
    private Boolean pruned = null;

    /**
     * Gets the role of the entity that created this message.
     * This is implemented by subclasses to provide compile-time type safety.
     *
     * @return The role of the message creator.
     */
    public abstract Role getRole();
    
    /**
     * Calculates the "depth" of this message, defined as its distance from the
     * most recent message in the chat history. The head message has a depth of 0.
     * @return The depth of the message, or -1 if not attached to a chat.
     */
    public int getDepth() {
        if (chat == null) {
            return -1;
        }
        // Optimized to fetch the history only once.
        List<AbstractMessage> history = chat.getContextManager().getHistory();
        int index = history.indexOf(this);
        if (index == -1) {
            return -1;
        }
        return history.size() - 1 - index;
    }
    
    /**
     * Convenience method to get the message content as a single string,
     * concatenating the text representation of all its parts.
     *
     * @return The concatenated text content.
     */
    public String asText() {
        return getParts().stream()
            .map(AbstractPart::asText)
            .collect(Collectors.joining());
    }
    
    /**
     * A simple getter for the EXPLICIT pruned state of this message.
     * This is used by child parts to break the circular dependency in pruning logic.
     * @return The explicit pruned state.
     */
    public Boolean isPruned() {
        return pruned;
    }
    
    /**
     * Calculates the EFFECTIVE pruned state of this message.
     * A message is effectively pruned if it was explicitly pruned OR if all of its
     * constituent parts are effectively pruned.
     * @return {@code true} if the message is effectively pruned.
     */
    public boolean isEffectivelyPruned() {
        // 1. Check the explicit flag on the message itself.
        if (Boolean.TRUE.equals(this.pruned)) {
            return true;
        }
        // 2. If not explicitly pruned, it is effectively pruned only if ALL its parts are.
        // Note: stream().allMatch() correctly returns true for an empty stream.
        return getParts().stream().allMatch(AbstractPart::isEffectivelyPruned);
    }
    
    /**
     * The definitive convenience method for adapters. It returns a filtered list
     * containing only the parts of this message that are not effectively pruned.
     * @return A new list of the visible parts.
     */
    public List<AbstractPart> getVisibleParts() {
        return getParts().stream()
            .filter(p -> !p.isEffectivelyPruned())
            .collect(Collectors.toList());
    }
}
