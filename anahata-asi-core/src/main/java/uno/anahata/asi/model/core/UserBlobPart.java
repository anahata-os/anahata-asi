/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import lombok.NonNull;
import uno.anahata.asi.internal.TikaUtils;

/**
 * A concrete {@link BlobPart} implementation for binary content provided by the user.
 * It publishes itself to the parent message only after full initialization.
 * 
 * @author anahata
 */
public class UserBlobPart extends BlobPart {

    /**
     * Constructs a new UserBlobPart and adds it to the parent message.
     * 
     * @param message The parent message.
     * @param mimeType The MIME type.
     * @param data The binary data.
     */
    UserBlobPart(@NonNull AbstractMessage message, @NonNull String mimeType, @NonNull byte[] data) {
        super(message, mimeType, data);
        
        // Leaf class publication
        message.addPart(this);
    }

    UserBlobPart(@NonNull AbstractMessage message, @NonNull String mimeType, @NonNull byte[] data, @NonNull Path sourcePath) {
        super(message, mimeType, data, sourcePath);
        
        // Leaf class publication
        message.addPart(this);
    }

    /**
     * A convenience factory method to create a UserBlobPart from a local file path.
     *
     * @param message The message this part belongs to.
     * @param path The path to the file.
     * @return A new UserBlobPart instance.
     * @throws IOException if there's an error reading the file.
     * @throws Exception if there's an error detecting the MIME type.
     */
    public static UserBlobPart from(@NonNull AbstractMessage message, @NonNull Path path) throws Exception {
        byte[] data = Files.readAllBytes(path);
        String mimeType = TikaUtils.detectMimeType(path.toFile());
        return new UserBlobPart(message, mimeType, data, path);
    }
    
    /**
     * A convenience factory method to create a UserBlobPart from a legacy {@link File} object.
     * @param message The message this part belongs to.
     * @param file The file to read.
     * @return A new UserBlobPart instance.
     * @throws Exception if there's an error reading the file or detecting the MIME type.
     */
    public static UserBlobPart from(@NonNull AbstractMessage message, @NonNull File file) throws Exception {
        return from(message, file.toPath());
    }
}
