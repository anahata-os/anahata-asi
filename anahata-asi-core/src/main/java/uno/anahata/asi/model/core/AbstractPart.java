/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.LinkedHashMap;
import java.util.Map;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.chat.ChatConfig;
import uno.anahata.asi.internal.TextUtils;

/**
 * The abstract base class for all components of a {@link AbstractMessage}.
 * This class is central to the V2 context management system, providing a rich,
 * self-contained model for intelligent, time-based pruning and full context awareness.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Setter
public abstract class AbstractPart extends BasicPropertyChangeSource {
    
    /**
     * A unique, sequential identifier assigned to this part when it is added to a chat.
     */
    @Schema(hidden = true)
    private long sequentialId;

    /**
     * A backward reference to the Message that contains this part.
     * This is for runtime convenience and is ignored during schema generation
     * to keep the public contract clean.
     */
    @JsonIgnore
    @Schema(hidden = true)
    private AbstractMessage message;

    /**
     * A three-state flag for explicit pruning control.
     * - {@code true}: This part is explicitly pruned and will be hidden.
     * - {@code false}: This part is "pinned" and never be auto-pruned.
     * - {@code null}: (Default) Auto-pruning is active based on {@code turnsToKeep}.
     */
    @Schema(hidden = true)
    private Boolean pruned = null;
    
    /**
     * An optional reason for why this part was pruned.
     */
    @Schema(hidden = true)
    private String prunedReason;

    /**
     * An explicit, instance-level override for the number of user turns this
     * part should remain in the active context. If {@code null}, the effective
     * value is determined by the part type's default, resolved via the
     * {@link #getDefaultTurnsToKeep()} template method.
     */
    @Schema(hidden = true)
    private Integer turnsToKeep = null;

    /**
     * The number of tokens this part consumes in the context window.
     * This value is typically set by the AI provider or estimated during part creation.
     */
    @Schema(hidden = true)
    private int tokenCount;

    /**
     * Constructs a new AbstractPart.
     * 
     * @param message The parent message.
     */
    public AbstractPart(@NonNull AbstractMessage message) {
        // We only assign the message reference here. Concrete leaf classes are 
        // responsible for calling message.addPart(this) as the very last step 
        // of their constructors to ensure they are fully initialized before 
        // being published to UI listeners.
        this.message = message;
    }
    
    /**
     * Sets the pruned state of this part and fires a property change event.
     * 
     * @param pruned The new pruned state.
     */
    public void setPruned(Boolean pruned) {
        Boolean oldPruned = this.pruned;
        this.pruned = pruned;
        propertyChangeSupport.firePropertyChange("pruned", oldPruned, pruned);
    }

    /**
     * Sets the token count and fires a property change event.
     * 
     * @param tokenCount The new token count.
     */
    public void setTokenCount(int tokenCount) {
        int oldTokenCount = this.tokenCount;
        this.tokenCount = tokenCount;
        propertyChangeSupport.firePropertyChange("tokenCount", oldTokenCount, tokenCount);
    }

    /**
     * Removes this part from its parent message and severs the bidirectional link.
     * 
     * @throws IllegalStateException if the part is not attached to a message.
     */
    public void remove() {
        if (message == null) {
            throw new IllegalStateException("Cannot remove a part that is not attached to a message.");
        }
        message.removePart(this);
    }
    
    /**
     * Calculates the EFFECTIVE pruned state of this part, now with "deep pinning" logic.
     * A part is effectively pruned if it was explicitly pruned, if its PARENT MESSAGE
     * was explicitly pruned, or if its time-to-live has expired. Crucially, it is
     * considered NOT pruned if it or its parent message is "pinned" (pruned=false).
     *
     * @return {@code true} if the part is effectively pruned, {@code false} otherwise.
     */
    @JsonIgnore
    @Schema(hidden = true)
    public boolean isEffectivelyPruned() {
        // --- PINNING LOGIC (takes precedence) ---
        // 1. Is the part itself explicitly pinned?
        if (Boolean.FALSE.equals(this.pruned)) {
            return false; // It's pinned, not pruned.
        }
        // 2. Is the parent message pinned? (Deep Pin)
        if (message != null && Boolean.FALSE.equals(message.isPruned())) {
            return false; // Inherits pin, not pruned.
        }

        // --- PRUNING LOGIC ---
        // 3. Is the part itself explicitly pruned?
        if (Boolean.TRUE.equals(this.pruned)) {
            return true;
        }
        // 4. Is the parent message explicitly pruned?
        if (message != null && Boolean.TRUE.equals(message.isPruned())) {
            return true;
        }
        // 5. Finally, check the time-based auto-pruning logic (only if pruned is null for both).
        if (getTurnsLeft() <= 0) {
            return true;
        }

        return false;
    }

    /**
     * Calculates the remaining turns before this part is auto-pruned.
     * 
     * @return The number of turns left, or a large positive number for indefinite retention.
     */
    @JsonIgnore
    @Schema(hidden = true)
    public int getTurnsLeft() {
        int effectiveTurns = getEffectiveTurnsToKeep();
        if (effectiveTurns < 0) {
            return Integer.MAX_VALUE; // Indefinite
        }
        return message != null ? effectiveTurns - message.getDepth() : effectiveTurns;
    }

    /**
     * The definitive method for resolving the retention policy for this part.
     * It follows the Template Method pattern, first checking for an explicit
     * instance-level override before falling back to the subclass-specific
     * default.
     * 
     * @return The effective number of turns to keep this part.
     */
    @JsonIgnore
    @Schema(hidden = true)
    public final int getEffectiveTurnsToKeep() {
        if (turnsToKeep != null) {
            return turnsToKeep;
        }
        return getDefaultTurnsToKeep();
    }

    /**
     * Template method hook for subclasses to provide their specific default
     * retention policy. This is the fallback value used when no explicit
     * {@code turnsToKeep} is set on the instance.
     * 
     * @return The default number of turns for this part type.
     */
    protected abstract int getDefaultTurnsToKeep();

    /**
     * Gets the parent chat session.
     * 
     * @return The chat session, or null if not attached to a message.
     */
    @JsonIgnore
    @Schema(hidden = true)
    public Chat getChat() {
        return message != null ? message.getChat() : null;
    }

    /**
     * Gets the chat configuration.
     * 
     * @return The chat configuration, or null if not attached to a chat.
     */
    @JsonIgnore
    @Schema(hidden = true)
    public ChatConfig getChatConfig() {
        Chat chat = getChat();
        return chat != null ? chat.getConfig() : null;
    }

    /**
     * Returns the content of the part as a simple string.
     * This is implemented by subclasses.
     * 
     * @return The text representation of the part.
     */
    public abstract String asText();

    /**
     * Creates a standardized text header containing metadata for this part.
     * This is used for in-band metadata injection to improve model self-awareness.
     * If the part is effectively pruned, it includes a descriptive hint to maintain
     * semantic context.
     * 
     * @return A formatted metadata header string.
     */
    public String createMetadataHeader() {
        StringBuilder sb = new StringBuilder();
        sb.append(String.format("[Part ID: %d | Type: %s | Tokens: %d | Turns Left: %d",
            getSequentialId(),
            getClass().getSimpleName(),
            getTokenCount(),
            getTurnsLeft()
        ));

        appendMetadata(sb);

        if (Boolean.TRUE.equals(pruned)) {
            sb.append(" | [PRUNED]");
        } else if (Boolean.FALSE.equals(pruned)) {
            sb.append(" | [PINNED]");
        }
        
        if (isEffectivelyPruned()) {
            if (prunedReason != null) {
                sb.append(" | Reason: ").append(prunedReason);
            }
            sb.append(" | Hint: ").append(TextUtils.formatValue(asText()));
        }
        
        sb.append("]");
        return sb.toString();
    }

    /**
     * Hook for subclasses to inject specialized metadata into the part header.
     * 
     * @param sb The StringBuilder building the header.
     */
    protected void appendMetadata(StringBuilder sb) {
        // Default implementation does nothing.
    }

    /**
     * Returns a map of metadata for this part, suitable for JSON injection.
     * 
     * @return A map containing part metadata.
     */
    @JsonIgnore
    @Schema(hidden = true)
    public Map<String, Object> getMetadataMap() {
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("partId", getSequentialId());
        metadata.put("type", getClass().getSimpleName());
        metadata.put("tokens", getTokenCount());
        metadata.put("turnsLeft", getTurnsLeft());
        if (pruned != null) {
            metadata.put("pruned", pruned);
        }
        if (prunedReason != null) {
            metadata.put("prunedReason", prunedReason);
        }
        return metadata;
    }
}
