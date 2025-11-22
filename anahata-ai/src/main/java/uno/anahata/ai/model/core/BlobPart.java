/* Licensed under the Anahata Software License, Version 108 - https://github.com/anahata-os/anahata-ai/blob/main/LICENSE */
package uno.anahata.ai.model.core;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.ai.internal.TikaUtils;

/**
 * Represents a binary data part, such as an image or a document.
 * It can be created from raw bytes or directly from a file path, in which case
 * it retains a reference to the source path for traceability.
 *
 * @author anahata-ai
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
    private final Path sourcePath;

    /**
     * Constructs a BlobPart from raw byte data and a specified MIME type.
     * The source path will be null.
     *
     * @param mimeType The MIME type.
     * @param data The binary data.
     */
    public BlobPart(@NonNull String mimeType, @NonNull byte[] data) {
        this.mimeType = mimeType;
        this.data = data;
        this.sourcePath = null;
    }
    
    private BlobPart(@NonNull String mimeType, @NonNull byte[] data, @NonNull Path sourcePath) {
        this.mimeType = mimeType;
        this.data = data;
        this.sourcePath = sourcePath;
    }

    /**
     * A convenience factory method to create a BlobPart from a local file path.
     * It automatically reads the file's bytes, detects its MIME type using Tika,
     * and stores a reference to the source path.
     *
     * @param path The path to the file.
     * @return A new BlobPart instance.
     * @throws IOException if there's an error reading the file.
     * @throws Exception if there's an error detecting the MIME type.
     */
    public static BlobPart from(@NonNull Path path) throws Exception {
        byte[] data = Files.readAllBytes(path);
        String mimeType = TikaUtils.detectMimeType(path.toFile());
        return new BlobPart(mimeType, data, path);
    }
    
    /**
     * A convenience factory method to create a BlobPart from a legacy {@link File} object.
     * @see #from(Path) 
     */
    public static BlobPart from(@NonNull File file) throws Exception {
        return from(file.toPath());
    }
    
    @Override
    public String asText() {
        String source = sourcePath != null ? ", source: " + sourcePath : "";
        return "[Blob: " + mimeType + ", " + data.length + " bytes" + source + "]";
    }
}