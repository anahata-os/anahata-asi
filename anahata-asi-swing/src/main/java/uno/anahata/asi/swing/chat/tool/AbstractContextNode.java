/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.List;
import javax.swing.Icon;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.model.tool.AbstractToolkit;
import uno.anahata.asi.model.tool.ToolPermission;
import uno.anahata.asi.swing.chat.ChatPanel;
import uno.anahata.asi.swing.chat.SwingChatConfig;
import uno.anahata.asi.swing.icons.IconProvider;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * The base class for all nodes in the hierarchical context tree.
 * <p>
 * This class provides a unified interface for the {@link ContextTreeTableModel} 
 * to interact with different types of domain objects (Providers, Toolkits, Tools) 
 * while maintaining a consistent JNDI-style view.
 * </p>
 * <p>
 * It implements a hierarchical token aggregation system, where each node 
 * calculates its own tokens and sums up the tokens of its children.
 * </p>
 *
 * @author anahata
 * @param <T> The type of the underlying domain object wrapped by this node.
 */
@Getter
@RequiredArgsConstructor
public abstract class AbstractContextNode<T> {

    /** The parent chat panel. */
    protected final ChatPanel chatPanel;

    /** 
     * The underlying domain object. 
     * @return the wrapped domain object.
     */
    protected final T userObject;

    /** Cached instruction token count (including children). */
    protected int instructionsTokens;
    /** Cached declaration token count (including children). */
    protected int declarationsTokens;
    /** Cached history token count (including children). */
    protected int historyTokens;
    /** Cached RAG token count (including children). */
    protected int ragTokens;
    /** Cached status string. */
    protected String status;

    /**
     * Gets the parent chat session.
     * @return The chat session.
     */
    public Chat getChat() {
        return chatPanel.getChat();
    }

    /**
     * Gets the human-readable display name for this node.
     * @return The name to be displayed in the tree.
     */
    public abstract String getName();

    /**
     * Gets a detailed description of the node's purpose or content.
     * @return The description string.
     */
    public abstract String getDescription();

    /**
     * Gets the list of child nodes for this node, implementing the 
     * hierarchical logic specific to the node type.
     * 
     * @return A list of child AbstractContextNodes.
     */
    public abstract List<AbstractContextNode<?>> getChildren();

    /**
     * Recalculates and caches the token counts and status for this node 
     * and all its descendants.
     * Implementation details: It resets counts, calls {@link #calculateLocalTokens()}, 
     * then recursively calls refreshTokens on children and aggregates their counts.
     */
    public final void refreshTokens() {
        this.instructionsTokens = 0;
        this.declarationsTokens = 0;
        this.historyTokens = 0;
        this.ragTokens = 0;
        
        calculateLocalTokens();
        
        for (AbstractContextNode<?> child : getChildren()) {
            child.refreshTokens();
            this.instructionsTokens += child.getInstructionsTokens();
            this.declarationsTokens += child.getDeclarationsTokens();
            this.historyTokens += child.getHistoryTokens();
            this.ragTokens += child.getRagTokens();
        }
        
        updateStatus();
    }
    
    /**
     * Calculates the token counts for this node's own content (excluding children).
     */
    protected abstract void calculateLocalTokens();
    
    /**
     * Updates the status string for this node.
     */
    protected abstract void updateStatus();
    
    /**
     * Gets an optional icon for this node by delegating to the 
     * {@link IconProvider} configured in the chat session.
     * 
     * @return The icon, or null if no specialized icon is available.
     */
    public Icon getIcon() {
        if (getChat().getConfig() instanceof SwingChatConfig scc) {
            IconProvider provider = scc.getIconProvider();
            if (provider != null) {
                // Check for more specific types first to avoid clobbering by ContextProvider check
                if (userObject instanceof AbstractTool<?, ?> tool) {
                    return provider.getIconFor(tool);
                } else if (userObject instanceof AbstractToolkit<?> tk) {
                    return provider.getIconFor(tk);
                } else if (userObject instanceof ContextProvider cp) {
                    return provider.getIconFor(cp);
                }
            }
        }
        return null;
    }

    /**
     * Checks if the underlying domain object is currently "active" in the context.
     * <p>
     * An object is inactive if it is a disabled toolkit, a provider not effectively 
     * providing (including parent state), a deleted resource, or a pruned message/part.
     * </p>
     * 
     * @return {@code true} if active, {@code false} otherwise.
     */
    public boolean isActive() {
        if (userObject instanceof ContextProvider cp) {
            boolean active = cp.isEffectivelyProviding();
            if (cp instanceof AbstractResource<?, ?> res) {
                active = active && res.exists();
            }
            return active;
        } else if (userObject instanceof AbstractToolkit<?> tk) {
            return tk.isEnabled() && tk.getToolManager().isEffectivelyProviding();
        } else if (userObject instanceof AbstractTool<?, ?> tool) {
            AbstractToolkit<?> tk = tool.getToolkit();
            return tool.getPermission() != ToolPermission.DENY_NEVER 
                && (tk == null || (tk.isEnabled() && tk.getToolManager().isEffectivelyProviding()));
        } else if (userObject instanceof AbstractMessage msg) {
            return !msg.isEffectivelyPruned();
        } else if (userObject instanceof AbstractPart part) {
            return !part.isEffectivelyPruned();
        }
        return true;
    }

    /**
     * {@inheritDoc}
     * Equality is based on the underlying userObject and the node class.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AbstractContextNode<?> that = (AbstractContextNode<?>) o;
        return userObject.equals(that.userObject);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return userObject.hashCode();
    }

    /**
     * {@inheritDoc}
     * Returns the display name of the node.
     */
    @Override
    public String toString() {
        return getName();
    }
}
