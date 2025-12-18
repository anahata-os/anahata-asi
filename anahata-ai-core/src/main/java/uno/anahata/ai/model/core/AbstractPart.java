/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.core;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.chat.ChatConfig;

/**
 * The abstract base class for all components of a {@link AbstractMessage}.
 * This class is central to the V3 context management system, providing a rich,
 * self-contained model for intelligent, time-based pruning and full context awareness.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Setter
public abstract class AbstractPart implements PropertyChangeSource {
    
    /** Support for firing property change events. */
    @JsonIgnore
    private final PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);

    /**
     * A unique, sequential identifier assigned to this part when it is added to a chat.
     */
    private long sequentialId;

    /**
     * A backward reference to the Message that contains this part.
     * This is for runtime convenience and is ignored during schema generation
     * to keep the public contract clean.
     */
    @JsonIgnore
    private AbstractMessage message;

    /**
     * A three-state flag for explicit pruning control.
     * - {@code true}: This part is explicitly pruned and will be hidden.
     * - {@code false}: This part is "pinned" and never be auto-pruned.
     * - {@code null}: (Default) Auto-pruning is active based on {@code turnsToKeep}.
     */
    private Boolean pruned = null;

    /**
     * An explicit, instance-level override for the number of user turns this
     * part should remain in the active context. If {@code null}, the effective
     * value is determined by the part type's default, resolved via the
     * {@link #getDefaultTurnsToKeep()} template method.
     */
    private Integer turnsToKeep = null;

    /**
     * Constructs a new AbstractPart and adds it to the parent message.
     * @param message The parent message.
     */
    public AbstractPart(@NonNull AbstractMessage message) {
        message.addPart(this);
        this.message = message;
    }
    
    /**
     * Sets the pruned state of this part and fires a property change event.
     * @param pruned The new pruned state.
     */
    public void setPruned(Boolean pruned) {
        Boolean oldPruned = this.pruned;
        this.pruned = pruned;
        // Fire a generic property change event without a specific property name
        propertyChangeSupport.firePropertyChange("pruned", oldPruned, pruned);
    }

    /**
     * Removes this part from its parent message and severs the bidirectional link.
     * This is the primary mechanism for removing attachments from a message.
     */
    public void remove() {
        message.removePart(this);
        this.message = null;
    }
    
    /**
     * Adds a PropertyChangeListener to this part.
     * @param listener The listener to add.
     */
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.addPropertyChangeListener(listener);
    }

    /**
     * Removes a PropertyChangeListener from this part.
     * @param listener The listener to remove.
     */
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.removePropertyChangeListener(listener);
    }

    /**
     * Calculates the EFFECTIVE pruned state of this part, now with "deep pinning" logic.
     * A part is effectively pruned if it was explicitly pruned, if its PARENT MESSAGE
     * was explicitly pruned, or if its time-to-live has expired. Crucially, it is
     * considered NOT pruned if it or its parent message is "pinned" (pruned=false).
     *
     * @return {@code true} if the part is effectively pruned, {@code false} otherwise.
     */
    public boolean isEffectivelyPruned() {
        // --- PINNING LOGIC (takes precedence) ---
        // 1. Is the part itself explicitly pinned?
        if (Boolean.FALSE.equals(this.pruned)) {
            return false; // It's pinned, not pruned.
        }
        // 2. Is the parent message pinned? (Deep Pin)
        if (Boolean.FALSE.equals(getMessage().isPruned())) {
            return false; // Inherits pin, not pruned.
        }

        // --- PRUNING LOGIC ---
        // 3. Is the part itself explicitly pruned?
        if (Boolean.TRUE.equals(this.pruned)) {
            return true;
        }
        // 4. Is the parent message explicitly pruned?
        if (Boolean.TRUE.equals(getMessage().isPruned())) {
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
     * @return The number of turns left, or a large positive number for indefinite retention.
     */
    public int getTurnsLeft() {
        int effectiveTurns = getEffectiveTurnsToKeep();
        if (effectiveTurns < 0) {
            return Integer.MAX_VALUE; // Indefinite
        }
        return effectiveTurns - getMessage().getDepth();
    }

    /**
     * The definitive method for resolving the retention policy for this part.
     * It follows the Template Method pattern, first checking for an explicit
     * instance-level override before falling back to the subclass-specific
     * default.
     * @return The effective number of turns to keep this part.
     */
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
     * @return The default number of turns for this part type.
     */
    protected abstract int getDefaultTurnsToKeep();

    /**
     * Gets the parent chat session.
     * @return The chat session.
     */
    @JsonIgnore
    public Chat getChat() {
        return getMessage().getChat();
    }

    /**
     * Gets the chat configuration.
     * @return The chat configuration.
     */
    @JsonIgnore
    public ChatConfig getChatConfig() {
        return getChat().getConfig();
    }

    /**
     * Returns the content of the part as a simple string.
     * This is implemented by subclasses.
     * @return The text representation of the part.
     */
    public abstract String asText();

    /** {@inheritDoc} */
    @Override
    public PropertyChangeSupport getPropertyChangeSupport() {
        return propertyChangeSupport;
    }
}
