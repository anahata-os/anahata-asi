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
 * Represents a binary data part, such as an image or a document.
 * It can be created from raw bytes or directly from a file path, in which case
 * it retains a reference to the source path for traceability.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public class BlobPart extends AbstractPart {

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
     * The source path will be null.
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
    
    private BlobPart(@NonNull AbstractMessage message, @NonNull String mimeType, @NonNull byte[] data, @NonNull Path sourcePath) {
        this(message, mimeType, data);
        this.sourcePath = sourcePath;
    }

    /**
     * A convenience factory method to create a BlobPart from a local file path.
     * It automatically reads the file's bytes, detects its MIME type using Tika,
     * and stores a reference to the source path.
     *
     * @param message The message this part belongs to.
     * @param path The path to the file.
     * @return A new BlobPart instance.
     * @throws IOException if there's an error reading the file.
     * @throws Exception if there's an error detecting the MIME type.
     */
    public static BlobPart from(@NonNull AbstractMessage message, @NonNull Path path) throws Exception {
        byte[] data = Files.readAllBytes(path);
        String mimeType = TikaUtils.detectMimeType(path.toFile());
        return new BlobPart(message, mimeType, data, path);
    }
    
    /**
     * A convenience factory method to create a BlobPart from a legacy {@link File} object.
     * @param message The message this part belongs to.
     * @param file The file to read.
     * @return A new BlobPart instance.
     * @throws Exception if there's an error reading the file or detecting the MIME type.
     * @see #from(AbstractMessage, Path) 
     */
    public static BlobPart from(@NonNull AbstractMessage message, @NonNull File file) throws Exception {
        return from(message, file.toPath());
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
