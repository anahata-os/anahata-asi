/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.provider;

/**
 * Exception thrown when an AI model generation call is interrupted, either
 * by the user or by the system.
 * 
 * @author anahata
 */
public class ApiCallInterruptedException extends RuntimeException {

    public ApiCallInterruptedException(String message) {
        super(message);
    }

    public ApiCallInterruptedException(String message, Throwable cause) {
        super(message, cause);
    }

    public ApiCallInterruptedException(Throwable cause) {
        super(cause);
    }
}
