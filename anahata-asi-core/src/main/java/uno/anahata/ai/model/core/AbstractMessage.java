/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.core;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
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
 * metadata and functionality for the rich, hierarchical V2 domain model. It
 * supports type-safe roles through its subclasses and ensures each message has
 * a unique identity, timestamp, and full access to the chat context.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public abstract class AbstractMessage implements PropertyChangeSource {

    /** Support for firing property change events. */
    private final PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);

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
    @Setter
    private long sequentialId;

    /**
     * The list of parts that make up the message content. Made private to
     * enforce encapsulation.
     */
    @Getter(AccessLevel.NONE)
    private final List<AbstractPart> parts = new ArrayList<>();

    /**
     * A backward reference to the Chat session that owns this message. This is
     * the root of the V2 context management system, allowing any domain object
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
     * Gets the identity of the sender of this message.
     * 
     * @return The sender's identity (e.g., user name or model ID).
     */
    public abstract String getFrom();
    
    /**
     * Whether this message is prunnable 
     * 
     * @return 
     */
    public boolean isPrunnableOrRemovable() {
        return getChat() != null && getSequentialId() != 0;
    }

    /**
     * Safely adds a single part to this message, establishing the bidirectional
     * relationship and ensuring the part is fully initialized before firing
     * property change events.
     *
     * @param part The part to add.
     * @throws IllegalArgumentException if the part is already attached to this or another message.
     */
    void addPart(@NonNull AbstractPart part) {
        if (parts.contains(part)) {
            throw new IllegalArgumentException("Part " + part + " is already a part of this message: " + this);
        }
        if (part.getMessage() != null && part.getMessage() != this) {
            throw new IllegalArgumentException("Part " + part + " already belongs to another message: " + part.getMessage());
        }
        
        // Establish the relationship BEFORE adding to the list and firing the event.
        // This ensures that UI listeners reacting to the "parts" change have access
        // to a fully initialized part object (including its parent message reference).
        part.setMessage(this);
        this.parts.add(part);
        propertyChangeSupport.firePropertyChange("parts", null, parts);
    }
    
    /**
     * Removes a part from this message and severs the bidirectional link.
     * 
     * @param part The part to remove.
     * @throws IllegalArgumentException if the part is not attached to this message.
     */
    void removePart(AbstractPart part) {
        Validate.isTrue(parts.contains(part), "Part " + part + " is not a part of this message.");
        parts.remove(part);
        part.setMessage(null);
        propertyChangeSupport.firePropertyChange("parts", null, parts);
    }
    
    /**
     * Removes this message from the chat history.
     */
    public void remove() {
        if (chat != null) {
            chat.getContextManager().removeMessage(this);
        }
    }

    /**
     * Sets the pruned state of this message and fires a property change event.
     * 
     * @param pruned The new pruned state.
     */
    public void setPruned(Boolean pruned) {
        Boolean oldPruned = this.pruned;
        this.pruned = pruned;
        propertyChangeSupport.firePropertyChange("pruned", oldPruned, pruned);
    }

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
     * @param includePruned whether to include pruned parts
     * @return The concatenated text content.
     */
    public String asText(boolean includePruned) {
        return getParts(includePruned).stream()
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
        if (Boolean.TRUE.equals(this.pruned)) {
            return true;
        }
        // A message is effectively pruned if it has parts and ALL of them are pruned.
        return getParts(false).isEmpty() && !parts.isEmpty();
    }

    /**
     * Determines if this message is eligible for "hard pruning" (permanent removal from history).
     * A message is generally considered garbage if it has no parts and is not explicitly pinned.
     * 
     * @return {@code true} if the message can be safely removed from history.
     */
    public boolean isGarbageCollectable() {
        return getParts(true).isEmpty() && !Boolean.FALSE.equals(pruned);
    }

    /**
     * The definitive, encapsulated method for retrieving the parts of a message
     * that should be sent to the model, respecting the pruning policy.
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

    /**
     * Gets an unmodifiable list of all parts in this message.
     * 
     * @return The list of parts.
     */
    public List<AbstractPart> getParts() {
        return Collections.unmodifiableList(parts);
    }
    
    /**
     * Adds a PropertyChangeListener to this message.
     * 
     * @param listener The listener to add.
     */
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.addPropertyChangeListener(listener);
    }

    /**
     * Removes a PropertyChangeListener from this message.
     * 
     * @param listener The listener to remove.
     */
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.removePropertyChangeListener(listener);
    }

    /** {@inheritDoc} */
    @Override
    public PropertyChangeSupport getPropertyChangeSupport() {
        return propertyChangeSupport;
    }

}
