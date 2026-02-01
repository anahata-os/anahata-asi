/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

/**
 * A concrete {@link TextPart} implementation for text content provided by the user.
 * It publishes itself to the parent message only after full initialization.
 * 
 * @author anahata
 */
public class UserTextPart extends TextPart {

    /**
     * Constructs a new UserTextPart and adds it to the parent message.
     * 
     * @param message The parent message.
     * @param text The text content.
     */
    UserTextPart(AbstractMessage message, String text) {
        super(message, text);
        
        // Leaf class publication
        message.addPart(this);
    }
}
