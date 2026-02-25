/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import org.apache.commons.lang3.Validate;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.internal.TextUtils;
import uno.anahata.asi.internal.TimeUtils;

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
public abstract class AbstractMessage extends BasicPropertyChangeSource {

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
     * The list of parts that make up the message content. Uses CopyOnWriteArrayList 
     * to allow thread-safe iteration during background serialization (e.g., auto-save) 
     * while the model is adding parts or appending text to existing parts.
     */
    @Getter(AccessLevel.NONE)
    private final List<AbstractPart> parts = new CopyOnWriteArrayList<>();

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
     * An optional reason for why this message was pruned.
     */
    @Setter
    private String prunedReason;

    /**
     * Gets the role of the entity that created this message. This is
     * implemented by subclasses to provide compile-time type safety.
     *
     * @return The role of the message creator.
     */
    public abstract Role getRole();

    /**
     * Gets the identity of the sender of this message.
     * For user messages, this is typically the user's name.
     * For model messages, it's the model ID.
     * For tool messages, it's the identity of the execution context.
     * 
     * @return The sender's identity.
     */
    public abstract String getFrom();
    
    /**
     * Gets the ID of the device where this message was created or processed.
     * This could be a hostname, a JVM ID, or a cloud identifier.
     * 
     * @return The device ID.
     */
    public abstract String getDevice();
    
    /**
     * Checks if this message is eligible for pruning or removal.
     * A message is prunnable if it is attached to a chat and has been assigned
     * a sequential ID (i.e., it's not the system message or a transient message).
     * 
     * @return {@code true} if the message is prunnable.
     */
    public boolean isPrunnableOrRemovable() {
        return getSequentialId() != 0;
    }

    /**
     * Safely adds a single part to this message, establishing the bidirectional
     * relationship and ensuring the part is fully initialized before firing
     * property change events.
     *
     * @param part The part to add.
     * @throws IllegalArgumentException if the part is already attached to this or another message.
     */
    public void addPart(@NonNull AbstractPart part) {
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

        // V2 ID Synchronization Fix: If the message is already identified (part of history),
        // we must identify the new part immediately to avoid sequentialId=0 issues.
        if (getSequentialId() != 0) {
            chat.getContextManager().identifyPart(part);
        }

        propertyChangeSupport.firePropertyChange("parts", null, parts);
    }
    
    /**
     * Removes a part from this message and severs the bidirectional link.
     * 
     * @param part The part to remove.
     * @throws IllegalArgumentException if the part is not attached to this message.
     */
    public void removePart(AbstractPart part) {
        Validate.isTrue(parts.contains(part), "Part " + part + " is not a part of this message.");
        parts.remove(part);
        part.setMessage(null);
        propertyChangeSupport.firePropertyChange("parts", null, parts);
    }
    
    /**
     * Removes this message from the chat history.
     */
    public void remove() {
        chat.getContextManager().removeMessage(this);
    }

    /**
     * Sets the pruned state of this message and fires a property change event.
     * 
     * @param pruned The new pruned state.
     */
    public void setPruned(Boolean pruned) {
        setPruned(pruned, null);
    }

    /**
     * Sets the pruned state of this message with an optional reason.
     * 
     * @param pruned The new pruned state.
     * @param reason The reason for pruning.
     */
    public void setPruned(Boolean pruned, String reason) {
        if (Objects.equals(this.pruned, pruned)) {
            return;
        }
        Boolean oldPruned = this.pruned;
        this.pruned = pruned;
        this.prunedReason = reason;
        propertyChangeSupport.firePropertyChange("pruned", oldPruned, pruned);
    }

    /**
     * Calculates the "depth" of this message, defined as its distance from the
     * most recent message in the chat history. The head message has a depth of
     * 0.
     *
     * @return The depth of the message.
     */
    public int getDepth() {
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
     * Checks if this message is explicitly pinned.
     * 
     * @return {@code true} if the message is pinned.
     */
    public boolean isPinned() {
        return Boolean.FALSE.equals(this.pruned);
    }

    /**
     * Calculates the EFFECTIVE pruned state of this message. 
     * A message is effectively pruned ONLY if it contains no visible (un-pruned) parts.
     * This logic ensures that if any part (like a pinned code block) remains visible,
     * the message itself remains effectively visible to host that part.
     *
     * @return {@code true} if the message is effectively pruned.
     */
    public boolean isEffectivelyPruned() {
        // If the turn is empty, it's effectively pruned regardless of the message flag.
        if (parts.isEmpty()) {
            return true;
        }
        
        // If there are ANY visible parts (not effectively pruned), the message is NOT effectively pruned.
        return getParts(false).isEmpty();
    }

    /**
     * Determines if this message is eligible for "hard pruning" (permanent removal from history).
     * A message is generally considered garbage if it has no parts and is not explicitly pinned.
     * 
     * @return {@code true} if the message can be safely removed from history.
     */
    public boolean isGarbageCollectable() {
        return getParts(true).isEmpty() && !isPinned();
    }

    /**
     * Calculates the total number of tokens in this message, summing the
     * token counts of its visible parts.
     * 
     * @param includePruned whether to include pruned parts
     * @return The total token count.
     */
    public int getTokenCount(boolean includePruned) {
        return getParts(includePruned).stream()
                .mapToInt(AbstractPart::getTokenCount)
                .sum();
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
     * Creates and adds a new text part to this message.
     * 
     * @param text The text content.
     * @return The created text part.
     */
    public abstract TextPart addTextPart(String text);

    /**
     * Creates and adds a new binary data part to this message.
     * 
     * @param mimeType The MIME type.
     * @param data The binary data.
     * @return The created blob part.
     */
    public abstract BlobPart addBlobPart(String mimeType, byte[] data);

    /**
     * Creates and adds a new binary data part from a local file path.
     * 
     * @param path The path to the file.
     * @return The created blob part.
     * @throws Exception if the file cannot be read or the MIME type cannot be detected.
     */
    public abstract BlobPart addBlobPart(java.nio.file.Path path) throws Exception;

    /**
     * Creates a standardized text header containing metadata for this message.
     * This is used for in-band metadata injection to improve model self-awareness.
     * 
     * @return A formatted metadata header string.
     */
    public String createMetadataHeader() {
        StringBuilder sb = new StringBuilder();
        sb.append("--- ");
        String identity = getIdentityLabel();
        if (identity != null && !identity.isEmpty()) {
            sb.append(identity).append(" | ");
        }
        sb.append(String.format("Role: %s | From: %s | Device: %s | Time: %s | Tokens: %d | Depth: %d",
            getRole(),
            getFrom(),
            getDevice(),
            TimeUtils.formatSmartTimestamp(Instant.ofEpochMilli(getTimestamp())),
            getTokenCount(false),
            getDepth()
        ));
        
        appendMetadata(sb);
        
        if (Boolean.TRUE.equals(pruned)) {
            sb.append(" | [PRUNED]");
            if (prunedReason != null) {
                sb.append(" Reason: ").append(prunedReason);
            }
        } else if (isPinned()) {
            sb.append(" | [PINNED]");
        } else {
            sb.append(" | [AUTO]");
        }
        
        sb.append(" ---");
        return sb.toString();
    }

    /**
     * Returns the identity label for the metadata header (e.g., "Message ID: 12").
     * Subclasses can override this to hide or customize the identity.
     * 
     * @return The identity label.
     */
    protected String getIdentityLabel() {
        return "Message ID: " + getSequentialId();
    }

    /**
     * Hook for subclasses to inject specialized metadata into the message header.
     * 
     * @param sb The StringBuilder building the header.
     */
    protected void appendMetadata(StringBuilder sb) {
        // Default implementation does nothing.
    }

    /**
     * Hook for subclasses to declare if they should generate in-band metadata headers.
     * 
     * @return {@code true} if metadata headers should be generated.
     */
    public boolean shouldCreateMetadata() {
        return true;
    }
}
