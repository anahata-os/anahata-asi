/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.gmail;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Builder;

/**
 * Metadata descriptor for an email attachment hosted on Gmail.
 *
 * @param attachmentId The unique attachment ID for binary retrieval via the Gmail API.
 * @param filename The original file name of the attachment.
 * @param mimeType The detected MIME type (e.g. {@code application/pdf}, {@code image/png}).
 * @param size The size of the attachment in bytes.
 *
 * @author anahata
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public record GmailAttachmentInfo(
        String attachmentId,
        String filename,
        String mimeType,
        long size
) {
}
