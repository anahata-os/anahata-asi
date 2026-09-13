/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.tool;

import java.nio.file.Path;
import lombok.Getter;
import org.apache.commons.io.FileUtils;
import uno.anahata.asi.Displayable;

/**
 * Represents a binary attachment associated with a tool's response.
 * This is used to capture non-textual output from tool execution, such as 
 * images, PDFs, or other data blobs.
 * 
 * @author anahata
 */
@Getter
public final class ToolResponseAttachment implements Displayable {
    
    /** The raw binary data of the attachment. */
    private final byte[] data;
    
    /** The MIME type of the data (e.g., "image/png"). */
    private final String mimeType;

    /** The source file path if this attachment was created from a local file. Can be null. */
    private final Path sourcePath;

    /**
     * Constructs a ToolResponseAttachment from raw byte data and MIME type.
     *
     * @param data The raw binary data of the attachment.
     * @param mimeType The MIME type of the data.
     */
    public ToolResponseAttachment(byte[] data, String mimeType) {
        this(data, mimeType, null);
    }

    /**
     * Constructs a ToolResponseAttachment with source file path traceability.
     *
     * @param data The raw binary data of the attachment.
     * @param mimeType The MIME type of the data.
     * @param sourcePath The original source path on disk, or null.
     */
    public ToolResponseAttachment(byte[] data, String mimeType, Path sourcePath) {
        this.data = data;
        this.mimeType = mimeType;
        this.sourcePath = sourcePath;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns a human-readable representation of this attachment, formatting the byte
     * size via {@link FileUtils#byteCountToDisplaySize(long)} and including the MIME type
     * and source path if present.
     * </p>
     */
    @Override
    public String toString() {
        String size = data != null ? FileUtils.byteCountToDisplaySize(data.length) : "0 bytes";
        StringBuilder sb = new StringBuilder();
        sb.append("Attachment [mimeType=").append(mimeType)
          .append(", size=").append(size);
        if (sourcePath != null) {
            sb.append(", sourcePath=").append(sourcePath);
        }
        sb.append("]");
        return sb.toString();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns a concise human-readable display string representing this attachment,
     * showing the file name (if available), formatted byte size, and MIME type.
     * </p>
     */
    @Override
    public String getDisplayValue() {
        String size = data != null ? FileUtils.byteCountToDisplaySize(data.length) : "0 bytes";
        if (sourcePath != null) {
            return sourcePath.toString() + " (" + size + ", " + mimeType + ")";
        }
        return mimeType + " (" + size + ")";
    }
}
