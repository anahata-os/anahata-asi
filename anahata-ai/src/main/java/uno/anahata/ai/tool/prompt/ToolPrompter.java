/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara! */
package uno.anahata.ai.tool.prompt;

import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.core.ModelMessage;

/**
 * The abstract contract for a component responsible for prompting the user
 * for confirmation before executing a batch of tool calls.
 *
 * @author anahata-gemini-pro-2.5
 */
public interface ToolPrompter {

    /**
     * Prompts the user for confirmation on a list of tool calls.
     *
     * @param modelMessage The message from the model containing the tool calls.
     * @param chat The current chat session.
     * @return A {@link PromptResult} containing the user's decisions and comments.
     */
    PromptResult prompt(ModelMessage modelMessage, Chat chat);
}
