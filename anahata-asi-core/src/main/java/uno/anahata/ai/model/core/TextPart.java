/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.core;

import lombok.Getter;
import lombok.Setter;

/**
 * A concrete {@link AbstractPart} implementation for simple text content.
 * It supports reactive UI updates by firing property change events when the text is modified.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public class TextPart extends AbstractPart {
    
    /** The text content of this part. */
    private String text;

    /**
     * Constructs a new TextPart.
     *
     * @param message The parent message.
     * @param text The initial text content.
     */
    public TextPart(AbstractMessage message, String text) {
        super(message);
        setText(text);
    }
    
    /**
     * Sets the text content and fires a property change event for the "text" property.
     * @param text The new text content.
     */
    public void setText(String text) {
        String oldText = this.text;
        this.text = text;
        getPropertyChangeSupport().firePropertyChange("text", oldText, text);
    }

    /**
     * Appends text to the existing content and fires a property change event.
     * 
     * @param delta The text to append.
     */
    public void appendText(String delta) {
        if (delta == null || delta.isEmpty()) {
            return;
        }
        String oldText = this.text;
        this.text = (this.text == null ? "" : this.text) + delta;
        getPropertyChangeSupport().firePropertyChange("text", oldText, this.text);
    }

    /** {@inheritDoc} */
    @Override
    public String asText() {
        return text;
    }

    /**
     * {@inheritDoc}
     * Returns the default number of turns to keep a text part in context,
     * as defined in the chat configuration.
     */
    @Override
    protected int getDefaultTurnsToKeep() {
        return getChatConfig().getDefaultTextPartTurnsToKeep();
    }
    
    /**
     * Indicates if this text part represents a model thought process.
     * For a generic TextPart, this is always false.
     * @return always false
     */
    public boolean isThought() {
        return false;
    }
}
