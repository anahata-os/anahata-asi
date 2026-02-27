/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

import java.nio.file.Path;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;

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
@Slf4j
public class InputUserMessage extends UserMessage {

    /**
     * The primary, editable text part of this message.
     */
    @Getter
    private TextPart editableTextPart;

    public InputUserMessage(Agi agi) {
        super(agi);
    }

    /**
     * Gets the text content of the primary editable part.
     *
     * @return The text content, or an empty string if no text part exists.
     */
    public String getText() {
        return editableTextPart != null ? editableTextPart.getText() : "";
    }

    /**
     * Sets the text content of the primary editable part.
     * If the text is empty, the text part is automatically removed from the message.
     *
     * @param text The new text content.
     */
    public void setText(String text) {
        if (text == null || text.trim().isEmpty()) {            
            if (editableTextPart != null) {
                editableTextPart.remove();
                editableTextPart = null;
            }
        } else {
            if (editableTextPart == null) {
                editableTextPart = new UserTextPart(this, text);
            } else {
                editableTextPart.setText(text);
            }
        }
    }

    /**
     * Checks if the message is empty. A message is considered empty if it
     * contains no parts.
     *
     * @return {@code true} if the message is empty, {@code false} otherwise.
     */
    public boolean isEmpty() {
        return getParts().isEmpty();
    }
    
    /**
     * Gets a list of all attached {@link BlobPart}s.
     * 
     * @return A list of BlobParts.
     */
    public List<BlobPart> getAttachments() {
        return getParts().stream()
            .filter(p -> p instanceof BlobPart)
            .map(p -> (BlobPart) p)
            .collect(Collectors.toList());
    }
    
    /**
     * Adds a single file path as an attachment to this message.
     * 
     * @param path The file path to attach.
     * @throws Exception if a BlobPart cannot be created from the path (e.g., file read error).
     */
    public void addAttachment(Path path) throws Exception {
        UserBlobPart.from(this, path);
    }

    /**
     * Adds a collection of file paths as attachments to this message.
     * 
     * @param paths The collection of file paths to attach.
     * @throws Exception if a BlobPart cannot be created from a path (e.g., file read error).
     */
    public void addAttachments(Collection<Path> paths) throws Exception {
        for (Path path : paths) {
            addAttachment(path); // Call the single-path method
        }
    }
}
