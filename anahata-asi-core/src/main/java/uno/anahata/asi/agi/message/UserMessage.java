/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.message;

import java.nio.file.Path;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.internal.TextUtils;

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
}
