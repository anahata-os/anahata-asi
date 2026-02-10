/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

import lombok.Getter;
import lombok.Setter;
import uno.anahata.asi.internal.TokenizerUtils;

/**
 * An abstract base class for text-based message parts.
 * It supports reactive UI updates by firing property change events when the text is modified.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Setter
public abstract class TextPart extends AbstractPart {
    
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
     * Also updates the token count.
     * @param text The new text content.
     */
    public void setText(String text) {
        String oldText = this.text;
        this.text = text;
        setTokenCount(TokenizerUtils.countTokens(text));
        propertyChangeSupport.firePropertyChange("text", oldText, text);
    }

    /**
     * Appends text to the existing content and fires a property change event.
     * Also updates the token count using an incremental estimate.
     * 
     * @param delta The text to append.
     */
    public void appendText(String delta) {
        if (delta == null || delta.isEmpty()) {
            return;
        }
        String oldText = this.text;
        this.text = (this.text == null ? "" : this.text) + delta;
        // Incremental estimation to avoid full recalculation on every chunk
        setTokenCount(getTokenCount() + TokenizerUtils.countTokens(delta));
        propertyChangeSupport.firePropertyChange("text", oldText, this.text);
    }

    /** {@inheritDoc} */
    @Override
    public String asText() {
        return text;
    }

    /**
     * {@inheritDoc}
     * Returns the default maximum depth to keep a text part in context,
     * as defined in the chat configuration.
     */
    @Override
    protected int getDefaultMaxDepth() {
        return getChatConfig().getDefaultTextPartMaxDepth();
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
