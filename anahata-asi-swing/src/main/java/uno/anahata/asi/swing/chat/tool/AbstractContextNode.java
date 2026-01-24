/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.List;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import uno.anahata.asi.chat.Chat;

/**
 * The base class for all nodes in the hierarchical context tree.
 * <p>
 * This class provides a unified interface for the {@link ContextTreeTableModel} 
 * to interact with different types of domain objects (Providers, Toolkits, Tools) 
 * while maintaining a consistent JNDI-style view.
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
     * Calculates the total token count for the system instructions 
     * contributed by this node and its descendants.
     * 
     * @param chat The active chat session.
     * @return The estimated instruction token count.
     */
    public abstract int getInstructionsTokens(Chat chat);

    /**
     * Calculates the total token count for the tool declarations 
     * associated with this node.
     * 
     * @return The estimated declaration token count.
     */
    public abstract int getDeclarationsTokens();

    /**
     * Calculates the total token count for the conversation history 
     * contributed by this node and its descendants.
     * 
     * @param chat The active chat session.
     * @return The estimated history token count.
     */
    public abstract int getHistoryTokens(Chat chat);

    /**
     * Calculates the total token count for the RAG (Retrieval-Augmented Generation) 
     * content contributed by this node and its descendants.
     * 
     * @param chat The active chat session.
     * @return The estimated RAG token count.
     */
    public abstract int getRagTokens(Chat chat);

    /**
     * Gets a string representation of the node's current operational status 
     * (e.g., "Active", "Enabled", "PROMPT").
     * 
     * @return The status string.
     */
    public abstract String getStatus();

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
