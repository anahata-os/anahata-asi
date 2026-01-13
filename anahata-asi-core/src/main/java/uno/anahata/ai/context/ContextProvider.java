/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara!
 */
package uno.anahata.ai.context;

import java.util.List;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.core.RagMessage;

/**
 * Defines the contract for providers that inject just-in-time context into an AI request.
 * Context providers can contribute system instructions or augment the user's prompt.
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
     * Checks if this context provider is currently enabled.
     * @return {@code true} if enabled, {@code false} otherwise.
     */
    boolean isEnabled();

    /**
     * Sets whether this context provider is enabled.
     * @param enabled {@code true} to enable, {@code false} to disable.
     */
    void setEnabled(boolean enabled);

    /**
     * Gets a list of system instruction parts provided by this context provider.
     * These are typically prepended to the conversation or sent as a separate parameter to the API.
     * @param chat the chat for which the system instruction parts are being produced
     * @return A list of system instruction strings.
     * @throws Exception if an error occurs during context generation.
     */
    List<String> getSystemInstructions(Chat chat) throws Exception;

    /**
     * Populates the given {@link RagMessage} with context parts from this provider.
     * These parts are appended to the end of the user's prompt as a synthetic UserMessage.
     * @param ragMessage The RagMessage to populate.
     * @throws Exception if an error occurs during context generation.
     */
    void populateMessage(RagMessage ragMessage) throws Exception;
    
    /**
     * Provides a machine readable header for this context provider
     * 
     * @return 
     */
    public default String getHeader() {
        return "### Context Provider Id:**" + getId() + "**\n"
                + "Name:" + getName()+ "\n"
                + "Description:" + getDescription()+ "\n"
                + "Enabled:" + isEnabled();
    }
}
