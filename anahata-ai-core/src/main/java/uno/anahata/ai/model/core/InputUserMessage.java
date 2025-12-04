package uno.anahata.ai.model.core;

import lombok.Getter;
import uno.anahata.ai.chat.Chat;

/**
 * A specialized UserMessage designed for direct manipulation by UI components like
 * an input panel.
 * <p>
 * This class encapsulates the logic of managing a primary, editable text part,
 * providing convenient methods to get and set its content. This avoids cluttering
 * the generic {@link UserMessage} with UI-specific concerns and prevents
 * unintended side effects on other subclasses like {@link RagMessage}.
 *
 * @author Anahata
 */
public class InputUserMessage extends UserMessage {

    /**
     * The primary, editable text part of this message.
     */
    @Getter
    private final TextPart editableTextPart;

    public InputUserMessage(Chat chat) {
        super(chat);
        this.editableTextPart = new TextPart(this, "");
    }

    /**
     * Gets the text content of the primary editable part.
     *
     * @return The text content.
     */
    public String getText() {
        return editableTextPart.getText();
    }

    /**
     * Sets the text content of the primary editable part.
     *
     * @param text The new text content.
     */
    public void setText(String text) {
        editableTextPart.setText(text);
    }

    /**
     * Checks if the message is empty. A message is considered empty if it
     * contains no parts other than the initial, empty editable text part.
     *
     * @return {@code true} if the message is empty, {@code false} otherwise.
     */
    public boolean isEmpty() {
        // It's empty if it only contains our editable text part AND that part is empty.
        return getParts().size() == 1 && editableTextPart.getText().isEmpty();
    }
}
