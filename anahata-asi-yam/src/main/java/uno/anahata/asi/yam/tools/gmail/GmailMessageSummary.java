/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.gmail;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Builder;

/**
 * Summary descriptor of a single email message returned by Gmail search queries.
 *
 * @param messageId The unique Gmail message identifier (e.g. {@code 18f1234abcd}).
 * @param threadId The thread ID this message belongs to.
 * @param date The message date header or formatted send timestamp.
 * @param from The sender address or name/address string.
 * @param to The recipient address or list of recipient addresses.
 * @param subject The email subject line.
 * @param hasAttachments Whether this message contains file attachments.
 * @param snippet A short text preview snippet extracted by Gmail.
 *
 * @author anahata
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public record GmailMessageSummary(
        String messageId,
        String threadId,
        String date,
        String from,
        String to,
        String subject,
        boolean hasAttachments,
        String snippet
) {
}
