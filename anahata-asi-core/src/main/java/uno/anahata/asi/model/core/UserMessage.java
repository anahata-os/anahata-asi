/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

import java.nio.file.Path;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.internal.TextUtils;

/**
 * Represents a message originating from the end-user in a conversation.
 * It extends the base {@link AbstractMessage} and sets its role to {@code USER}.
 *
 * @author anahata-gemini-pro-2.5
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
    
    /** {@inheritDoc} */
    @Override
    public Role getRole() {
        return Role.USER;
    }

    /**
     * {@inheritDoc}
     * Returns the user's identity as 'username@device'.
     */
    @Override
    public String getFrom() {
        return TextUtils.getUserName() + "@" + TextUtils.getDeviceId();
    }

    /** {@inheritDoc} */
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
}
