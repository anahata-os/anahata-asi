/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

import java.time.Instant;
import java.util.Objects;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.internal.TextUtils;

/**
 * The abstract base class for all components of a {@link AbstractMessage}.
 * This class is central to the V2 context management system, providing a rich,
 * self-contained model for intelligent, depth-based pruning and full context awareness.
 *
 * @author anahata
 */
@Getter
@Setter
public abstract class AbstractPart extends BasicPropertyChangeSource {
    
    /**
     * A unique, sequential identifier assigned to this part when it is added to a agi.
     */
    private long sequentialId;

    /**
     * A backward reference to the Message that contains this part.
     */
    private AbstractMessage message;

    /**
     * The pruning and pinning state of this part.
     */
    private PruningState pruningState = PruningState.AUTO;
    
    /**
     * An optional reason for why this part was pruned.
     */
    private String prunedReason;

    /**
     * An explicit, instance-level override for the maximum depth this
     * part should remain in the active context. If {@code null}, the effective
     * value is determined by the part type's default, resolved via the
     * {@link #getDefaultMaxDepth()} template method.
     */
    private Integer maxDepth = null;

    /**
     * The number of tokens this part consumes in the context window.
     * This value is typically set by the AI provider or estimated during part creation.
     */
    private int tokenCount;

    /**
     * Persistent UI state indicating if this part's panel is expanded in the conversation view.
     */
    private boolean expanded = true;

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
     * Sets the pruning state of this part and fires a property change event.
     * 
     * @param pruningState The new pruning state.
     */
    public void setPruningState(PruningState pruningState) {
        setPruningState(pruningState, null);
    }

    /**
     * Sets the pruning state of this part with an optional reason.
     * 
     * @param pruningState The new pruning state.
     * @param reason The reason for pruning.
     */
    public void setPruningState(PruningState pruningState, String reason) {
        PruningState oldState = this.pruningState;
        this.pruningState = pruningState;
        this.prunedReason = reason;
        propertyChangeSupport.firePropertyChange("pruningState", oldState, pruningState);
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
     * Sets the expanded state and fires a property change event.
     * 
     * @param expanded The new expanded state.
     */
    public void setExpanded(boolean expanded) {
        boolean oldExpanded = this.expanded;
        this.expanded = expanded;
        propertyChangeSupport.firePropertyChange("expanded", oldExpanded, expanded);
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
     * Checks if this part is explicitly pinned.
     * 
     * @return {@code true} if the part is pinned.
     */
    public boolean isPinned() {
        return pruningState == PruningState.PINNED;
    }

    /**
     * Checks if this part is managed automatically.
     * 
     * @return {@code true} if the part is in AUTO state.
     */
    public boolean isAuto() {
        return pruningState == PruningState.AUTO;
    }

    /**
     * Checks if this part is explicitly pruned.
     * 
     * @return {@code true} if the part is PRUNED.
     */
    public boolean isPruned() {
        return pruningState == PruningState.PRUNED;
    }

    /**
     * Calculates the EFFECTIVE pruned state of this part. 
     * A part is effectively pruned if it was explicitly pruned or if its 
     * time-to-live has expired (while in AUTO state).
     *
     * @return {@code true} if the part is effectively pruned, {@code false} otherwise.
     */
    public boolean isEffectivelyPruned() {
        if (isPinned()) {
            return false;
        }
        if (isPruned()) {
            return true;
        }
        return isAuto() && getRemainingDepth() <= 0;
    }

    /**
     * Determines if this part is eligible for "hard pruning" (permanent removal from history).
     * In the atomic model, a part is generally only collectable if IT and ALL its 
     * siblings in the message are effectively pruned.
     * 
     * @return {@code true} if the part can be safely removed from history.
     */
    public boolean isGarbageCollectable() {
        return isEffectivelyPruned() && getMessage().isEffectivelyPruned();
    }

    /**
     * Calculates the remaining depth before this part is auto-pruned.
     * 
     * @return The remaining depth, or a large positive number for indefinite retention.
     */
    public int getRemainingDepth() {
        int effectiveMaxDepth = getEffectiveMaxDepth();
        if (effectiveMaxDepth < 0) {
            return Integer.MAX_VALUE; // Indefinite
        }
        return message != null ? effectiveMaxDepth - message.getDepth() : effectiveMaxDepth;
    }

    /**
     * The definitive method for resolving the max depth policy for this part.
     * It follows the Template Method pattern, first checking for an explicit
     * instance-level override before falling back to the subclass-specific
     * default.
     * 
     * @return The effective maximum depth for this part.
     */
    public final int getEffectiveMaxDepth() {
        if (maxDepth != null) {
            return maxDepth;
        }
        return getDefaultMaxDepth();
    }

    /**
     * Template method hook for subclasses to provide their specific default
     * max depth policy. This is the fallback value used when no explicit
     * {@code maxDepth} is set on the instance.
     * 
     * @return The default maximum depth for this part type.
     */
    protected abstract int getDefaultMaxDepth();

    /**
     * Gets the parent agi session.
     * 
     * @return The agi session, or null if not attached to a message.
     */
    public Agi getAgi() {
        return message != null ? message.getAgi() : null;
    }

    /**
     * Gets the agi configuration.
     * 
     * @return The agi configuration, or null if not attached to a agi.
     */
    public AgiConfig getAgiConfig() {
        Agi agi = getAgi();
        return agi != null ? agi.getConfig() : null;
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
        sb.append("[");
        String identity = getIdentityLabel();
        if (identity != null && !identity.isEmpty()) {
            sb.append(identity).append(" | ");
        }
        sb.append(String.format("Type: %s | Tokens: %d",
            getClass().getSimpleName(),
            getTokenCount()
        ));
        
        int rd = getRemainingDepth();
        if (rd != Integer.MAX_VALUE) {
            sb.append(" | Remaining Depth: ").append(rd);
        }

        appendMetadata(sb);

        sb.append(" | pruningState: ").append(pruningState);
        if (isAuto()) {
            sb.append(" | effectivelyPruned: ").append(isEffectivelyPruned());
        }
        
        sb.append(" | expanded: ").append(expanded);
        
        if (isEffectivelyPruned()) {
            sb.append(" | Hint: ").append(TextUtils.formatValue(asText()));
        }
        
        sb.append("]");
        return sb.toString();
    }

    /**
     * Returns the identity label for the metadata header (e.g., "Part ID: 45").
     * Subclasses can override this to hide or customize the identity.
     * 
     * @return The identity label.
     */
    protected String getIdentityLabel() {
        return "x-anahata-part-id: " + getSequentialId();
    }

    /**
     * Hook for subclasses to inject specialized metadata into the part header.
     * 
     * @param sb The StringBuilder building the header.
     */
    protected void appendMetadata(StringBuilder sb) {
        // Default implementation does nothing.
    }
}
