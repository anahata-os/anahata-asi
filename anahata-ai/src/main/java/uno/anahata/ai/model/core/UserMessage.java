/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.core;

/**
 * Represents a message originating from the end-user in a conversation.
 * It extends the base {@link AbstractMessage} and sets its role to {@code USER}.
 *
 * @author anahata-gemini-pro-2.5
 */
public class UserMessage extends AbstractMessage {
    @Override
    public Role getRole() {
        return Role.USER;
    }
}
