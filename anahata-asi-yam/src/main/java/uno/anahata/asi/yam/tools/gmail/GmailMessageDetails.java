/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.gmail;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.util.List;
import lombok.Builder;

/**
 * Detailed representation of an email message including full RFC 822 headers,
 * decoded text and HTML bodies, and attachment descriptors.
 *
 * @param messageId The unique Gmail message identifier.
 * @param threadId The parent thread ID.
 * @param date The date string from the message headers.
 * @param from The sender email address or formatted identity.
 * @param to The recipient email addresses.
 * @param cc The carbon copy recipients (if present).
 * @param bcc The blind carbon copy recipients (if present).
 * @param subject The email subject.
 * @param snippet The snippet preview.
 * @param bodyPlain The decoded plain text content.
 * @param bodyHtml The decoded or sanitized HTML content.
 * @param hasAttachments Whether file attachments exist.
 * @param attachments List of attachments with metadata and retrieval IDs.
 * @param labels List of Gmail system or user labels applied to the message.
 *
 * @author anahata
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public record GmailMessageDetails(
        String messageId,
        String threadId,
        String date,
        String from,
        String to,
        String cc,
        String bcc,
        String subject,
        String snippet,
        String bodyPlain,
        String bodyHtml,
        boolean hasAttachments,
        List<GmailAttachmentInfo> attachments,
        List<String> labels
) {
}
