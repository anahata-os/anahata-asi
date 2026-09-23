/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.message;

import java.nio.file.Path;
import java.util.stream.Collectors;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.internal.TextUtils;
import lombok.NonNull;

/**
 * Represents a conversation message originating from the end-user.
 * <p>
 * This class is the primary entry point for human intent within the Agi session. 
 * It extends {@link AbstractMessage} to provide user-specific identity 
 * resolution and ensures that all user-provided content is correctly typed 
 * and attributed to the host device.
 * </p>
 *
 * @author anahata
 */
public class UserMessage extends AbstractMessage {

    /**
     * Constructs a new UserMessage.
     * 
     * @param agi The parent agi session.
     */
    public UserMessage(Agi agi) {
        super(agi);
    }
    
    /** 
     * {@inheritDoc} 
     * <p>Returns the {@link Role#USER} constant for this message type.</p> 
     */
    @Override
    public Role getRole() {
        return Role.USER;
    }

    /**
     * {@inheritDoc}
     * <p>Returns the user's identity formatted as 'username@device'.</p>
     */
    @Override
    public String getFrom() {
        return TextUtils.getUserName() + "@" + TextUtils.getDeviceId();
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the unique identifier of the host that originated this user message.</p> 
     */
    @Override
    public String getDevice() {
        return TextUtils.getDeviceId();
    }

    /**
     * {@inheritDoc}
     * Creates and adds a {@link UserTextPart}.
     * 
     * @param text The text content.
     * @return The created text part.
     */
    @Override
    public final TextPart addTextPart(String text) {
        return new UserTextPart(this, text);
    }

    /**
     * {@inheritDoc}
     * Creates and adds a {@link UserBlobPart}.
     * 
     * @param mimeType The MIME type.
     * @param data The binary data.
     * @return The created blob part.
     */
    @Override
    public final BlobPart addBlobPart(String mimeType, byte[] data) {
        return new UserBlobPart(this, mimeType, data);
    }

    /**
     * {@inheritDoc}
     * Creates and adds a {@link UserBlobPart} from a file path.
     * 
     * @param path The file path.
     * @return The created blob part.
     * @throws Exception if the file cannot be read.
     */
    @Override
    public final BlobPart addBlobPart(Path path) throws Exception {
        return UserBlobPart.from(this, path);
    }

    /**
     * Appends the text and attachment content of another user message into this
     * message.
     *
     * @param other The incoming user message to merge into this one.
     */
    public void append(@NonNull UserMessage other) {
        for (AbstractPart part : other.getParts()) {
            if (part instanceof TextPart tp && tp.getText() != null && !tp.getText().isBlank()) {
                addTextPart(tp.getText());
            } else if (part instanceof BlobPart bp) {
                addBlobPart(bp.getMimeType(), bp.getData());
            }
        }
    }

    /**
     * Concatenates the text content of all {@link TextPart}s in this message,
     * excluding binary attachments and tool representations.
     *
     * @return The combined text of all text parts, or an empty string if no
     * text exists.
     */
    public String getAllText() {
        return getParts().stream()
                .filter(TextPart.class::isInstance)
                .map(p -> ((TextPart) p).getText())
                .filter(t -> t != null && !t.isBlank())
                .collect(Collectors.joining("\n\n"));
    }

    /**
     * Generates a concise, single-line summary of the message suitable for UI
     * previews, window titles, or staged message badges. Collapses all newlines
     * and whitespace into single spaces, truncates text, and appends attachment
     * counts.
     *
     * @param maxTextLength The maximum number of text characters before
     * truncating with "...".
     * @return A clean, formatted summary string (e.g. "message1 message2 (+2
     * attachments)").
     */
    public String getBriefSummary(int maxTextLength) {
        String text = getAllText().replaceAll("\\s+", " ").trim();
        if (text.length() > maxTextLength) {
            text = text.substring(0, Math.max(0, maxTextLength - 3)).trim() + "...";
        }
        long attachmentCount = getParts().stream().filter(BlobPart.class::isInstance).count();
        String suffix = attachmentCount > 0 ? " (+" + attachmentCount + (attachmentCount == 1 ? " attachment)" : " attachments)") : "";

        if (text.isEmpty() && attachmentCount > 0) {
            return attachmentCount + (attachmentCount == 1 ? " attachment" : " attachments");
        }
        return text + suffix;
    }

    /**
     * Generates a concise summary of this message defaulting to a 50-character
     * text limit.
     *
     * @return A formatted summary string.
     */
    public String getBriefSummary() {
        return getBriefSummary(50);
    }
}
