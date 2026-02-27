/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.tool;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * Represents a binary attachment associated with a tool's response.
 * This is used to capture non-textual output from tool execution, such as 
 * images, PDFs, or other data blobs.
 * 
 * @author anahata
 */
@RequiredArgsConstructor
@Getter
public final class ToolResponseAttachment {
    
    /** The raw binary data of the attachment. */
    private final byte[] data;
    
    /** The MIME type of the data (e.g., "image/png"). */
    private final String mimeType;
}
