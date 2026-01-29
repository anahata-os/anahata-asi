/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.List;
import javax.swing.Icon;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * The base class for all nodes in the hierarchical context tree.
 * <p>
 * This class provides a unified interface for the {@link ContextTreeTableModel} 
 * to interact with different types of domain objects (Providers, Toolkits, Tools) 
 * while maintaining a consistent JNDI-style view.
 * </p>
 * <p>
 * <b>Performance Note:</b> Token counts and status are cached as fields to ensure 
 * that the {@code getValueAt} method in the TreeTableModel remains O(1) and 
 * does not trigger expensive tokenization during UI rendering.
 * </p>
 *
 * @author anahata
 * @param <T> The type of the underlying domain object wrapped by this node.
 */
@Getter
@RequiredArgsConstructor
public abstract class AbstractContextNode<T> {

    /** 
     * The underlying domain object. 
     * @return the wrapped domain object.
     */
    protected final T userObject;

    /** Cached instruction token count. */
    protected int instructionsTokens;
    /** Cached declaration token count. */
    protected int declarationsTokens;
    /** Cached history token count. */
    protected int historyTokens;
    /** Cached RAG token count. */
    protected int ragTokens;
    /** Cached status string. */
    protected String status;

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
     * <p>
     * This method should be called explicitly (e.g., via a refresh button) 
     * to update the "snapshot" of the context's token usage.
     * </p>
     */
    public abstract void refreshTokens();
    
    /**
     * Gets an optional icon for this node.
     * @return The icon, or null if no specialized icon is available.
     */
    public Icon getIcon() {
        return null;
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
}
