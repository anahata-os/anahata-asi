/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.tool;

/**
 * A custom exception that can be thrown by AI tools to provide a concise,
 * user-friendly error message to the model without including a stack trace.
 *
 * @author anahata-gemini-pro-2.5
 */
public class AiToolException extends Exception {

    public AiToolException(String message) {
        super(message);
    }

    public AiToolException(String message, Throwable cause) {
        super(message, cause);
    }
}
