/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.asi.internal.TikaUtils;

/**
 * An abstract base class for binary data parts, such as an image or a document.
 * It can be created from raw bytes or directly from a file path, in which case
 * it retains a reference to the source path for traceability.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public abstract class BlobPart extends AbstractPart {

    /** The MIME type of the data (e.g., "image/png", "application/pdf"). */
    @NonNull
    private final String mimeType;

    /** The raw binary data. */
    @NonNull
    private final byte[] data;
    
    /** The original source path if this blob was created from a file. Can be null. */
    private Path sourcePath;

    /**
     * Constructs a BlobPart from raw byte data and a specified MIME type.
     *
     * @param message The message this part belongs to.
     * @param mimeType The MIME type.
     * @param data The binary data.
     */
    public BlobPart(@NonNull AbstractMessage message, @NonNull String mimeType, @NonNull byte[] data) {
        super(message);
        this.mimeType = mimeType;
        this.data = data;
    }
    
    /**
     * Internal constructor for BlobParts with a source path.
     * 
     * @param message The parent message.
     * @param mimeType The MIME type.
     * @param data The binary data.
     * @param sourcePath The source file path.
     */
    protected BlobPart(@NonNull AbstractMessage message, @NonNull String mimeType, @NonNull byte[] data, @NonNull Path sourcePath) {
        this(message, mimeType, data);
        this.sourcePath = sourcePath;
    }

    @Override
    public String asText() {
        String source = sourcePath != null ? ", source: " + sourcePath : "";
        return "[Blob: " + mimeType + ", " + data.length + " bytes" + source + "]";
    }

    @Override
    protected int getDefaultTurnsToKeep() {
        return getChatConfig().getDefaultBlobPartTurnsToKeep();
    }
}
