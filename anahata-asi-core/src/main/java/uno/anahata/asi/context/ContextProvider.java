/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.context;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import uno.anahata.asi.internal.TokenizerUtils;
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
     * Checks if this provider is effectively providing context, meaning it is 
     * enabled AND all its ancestors in the hierarchy are also enabled.
     * 
     * @return {@code true} if effectively providing, {@code false} otherwise.
     */
    default boolean isEffectivelyProviding() {
        if (!isProviding()) return false;
        ContextProvider parent = getParentProvider();
        return parent == null || parent.isEffectivelyProviding();
    }

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
     * @return A list of system instruction strings.
     * @throws Exception if an error occurs during instruction generation.
     */
    default List<String> getSystemInstructions() throws Exception {
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
        return "### Context Provider Id:**" + getFullyQualifiedId() + "**\n"
                + "Name: " + getName() + "\n"
                + "Description: " + getDescription() + "\n"
                + "Parent: " + (getParentProvider() != null ? getParentProvider().getFullyQualifiedId() : "<no parent provider>") + "\n"
                + "Children: " + getChildrenProviders().size() + "\n"
                + "Providing: " + isProviding() + "\n"
                + "Effectively Providing: " + isEffectivelyProviding() + "\n";
    }

    /**
     * Calculates the token count for the system instructions provided by this instance.
     * 
     * @return The estimated token count.
     */
    default int getInstructionsTokenCount() {
        try {
            List<String> instructions = getSystemInstructions();
            if (instructions.isEmpty()) return 0;
            int count = TokenizerUtils.countTokens(getHeader());
            for (String s : instructions) {
                count += TokenizerUtils.countTokens(s);
            }
            return count;
        } catch (Exception e) {
            return 0;
        }
    }

    /**
     * Calculates the token count for the RAG content provided by this instance.
     * 
     * @return The estimated token count.
     */
    default int getRagTokenCount() {
        try {
            // Note: This is a rough estimation. Concrete implementations should 
            // override this if they can provide a more accurate count without 
            // side effects.
            return 0; 
        } catch (Exception e) {
            return 0;
        }
    }

    /**
     * Gets an optional icon identifier for this provider.
     * This ID can be used by the UI to look up a specialized icon.
     * 
     * @return The provider's icon ID, or null if none.
     */
    default String getIconId() {
        return null;
    }
}
