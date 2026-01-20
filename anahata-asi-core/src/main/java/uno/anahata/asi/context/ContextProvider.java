/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.context;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.model.core.RagMessage;

/**
 * Defines the contract for providers that inject just-in-time context into an AI request.
 * Context providers can contribute system instructions or augment the user's prompt
 * through a hierarchical tree structure.
 *
 * @author anahata-ai
 */
public interface ContextProvider {

    /**
     * Gets the unique identifier for this context provider.
     * @return The provider's ID.
     */
    String getId();

    /**
     * Gets the human-readable name of this context provider.
     * @return The provider's name.
     */
    String getName();

    /**
     * Gets a detailed description of what this context provider does.
     * @return The provider's description.
     */
    String getDescription();

    /**
     * Checks if this context provider is currently active and providing context.
     * @return {@code true} if providing, {@code false} otherwise.
     */
    boolean isProviding();

    /**
     * Sets whether this context provider is enabled.
     * 
     * @param enabled {@code true} to enable, {@code false} to disable.
     */
    default void setProviding(boolean enabled) {
        
    }

    /**
     * Gets the parent context provider in the hierarchy, if any.
     * @return The parent provider, or null if this is a root provider.
     */
    default ContextProvider getParentProvider() {
        return null;
    }

    /**
     * Sets the parent context provider for this instance.
     * @param parent The parent provider.
     */
    default void setParentProvider(ContextProvider parent) {
        // Default implementation does nothing.
    }

    /**
     * Gets the list of immediate child context providers.
     * @return The list of children, or an empty list if none.
     */
    default List<ContextProvider> getChildrenProviders() {
        return Collections.emptyList();
    }
    
    /**
     * Gets the fully qualified ID of this provider, reflecting its position in the hierarchy.
     * The format is typically 'parent.child.id'.
     * 
     * @return The dot-separated fully qualified ID.
     */
    public default String getFullyQualifiedId() {
        return (getParentProvider() != null ? getParentProvider().getFullyQualifiedId() + "." : "") + getId();
    }

    /**
     * Gets a flattened list of this provider and all its descendants in the hierarchy.
     * 
     * @param enabledOnly if true, only providers where {@link #isProviding()} is true are included.
     * @return A flat list of the provider hierarchy.
     */
    default List<ContextProvider> getFlattenedHierarchy(boolean enabledOnly) {
        List<ContextProvider> list = new ArrayList<>();
        if (isProviding() || !enabledOnly) {
            list.add(this);
        } 
        
        for (ContextProvider child : getChildrenProviders()) {
            if (child.isProviding() || !enabledOnly) {
                list.addAll(child.getFlattenedHierarchy(enabledOnly));
            }
        }
        return list;
    }

    /**
     * Gets a list of system instruction strings provided by this context provider.
     * These are typically prepended to the conversation as high-level guidance.
     * 
     * @param chat The chat session for which the instructions are being generated.
     * @return A list of system instruction strings.
     * @throws Exception if an error occurs during instruction generation.
     */
    default List<String> getSystemInstructions(Chat chat) throws Exception {
        return Collections.EMPTY_LIST;
    }

    /**
     * Populates the given {@link RagMessage} with dynamic, just-in-time context parts.
     * These parts are appended to the end of the user's prompt (RAG).
     * 
     * @param ragMessage The message to be augmented with context.
     * @throws Exception if an error occurs during context generation.
     */
    default void populateMessage(RagMessage ragMessage) throws Exception {
        
    }
    
    /**
     * Generates a machine-readable header string for this context provider,
     * used to identify the source of injected context in the final prompt.
     * 
     * @return A formatted header string.
     */
    public default String getHeader() {
        String header =
                "### Context Provider Id:**" + getFullyQualifiedId() + "**\n"                
                + "Name:" + getName()+ "\n"
                + "Description:" + getDescription()+ "\n"                
                + "Children:" + getChildrenProviders().size() + "\n"
                + "Providing:" + isProviding() + "\n";
                return header;
    }
}
