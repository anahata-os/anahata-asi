/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.tool;

/**
 * A custom runtime exception that can be thrown by AI tools to provide a concise,
 * user-friendly error message to the model without including a stack trace.
 * <p>
 * Using a RuntimeException simplifies tool signatures by removing the need for 
 * checked exception declarations.
 * </p>
 *
 * @author anahata
 */
public class AiToolException extends RuntimeException {

    public AiToolException(String message) {
        super(message);
    }

    public AiToolException(String message, Throwable cause) {
        super(message, cause);
    }
}
