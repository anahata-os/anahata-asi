package uno.anahata.asi.agi.provider;

/**
 * Generic runtime exception for AiProvider implementations.
 * @author anahata
 */
public class AiProviderException extends RuntimeException {

    /**
     * Creates a new instance of <code>AiProviderException</code> without detail
     * message.
     */
    public AiProviderException() {
    }

    /**
     * Constructs an instance of <code>AiProviderException</code> with the
     * specified detail message.
     *
     * @param msg the detail message.
     */
    public AiProviderException(String msg) {
        super(msg);
    }
}
