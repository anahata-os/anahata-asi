/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.message;

import java.nio.file.Path;
import lombok.Getter;
import lombok.NonNull;

/**
 * An abstract base class for binary data parts, such as an image or a document.
 * It can be created from raw bytes or directly from a file path, in which case
 * it retains a reference to the source path for traceability.
 *
 * @author anahata
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

    /**
     * {@inheritDoc}
     * Provides essential binary metadata (MIME and Size) when the content is pruned.
     */
    @Override
    public String getPrunedHint() {
        return String.format("Mime: %s | Size: %d bytes", mimeType, data.length);
    }

    /**
     * {@inheritDoc}
     * Always includes the source path in the metadata header if available,
     * ensuring semantic continuity even when pruned.
     */
    @Override
    protected void appendMetadata(StringBuilder sb) {
        if (sourcePath != null) {
            sb.append(" | Path: ").append(sourcePath.toString());
        }
    }

    @Override
    protected int getDefaultMaxDepth() {
        return getAgiConfig().getDefaultBlobPartMaxDepth();
    }
}
