/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.vcs;

import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serializable;
import java.time.Instant;
import java.util.List;
import uno.anahata.asi.internal.TimeUtils;

/**
 * Universal DTO representing a single version control commit or IDE local
 * history entry.
 *
 * @param timestamp Epoch timestamp in milliseconds.
 * @param dateFormatted Human-readable formatted date string.
 * @param source The origin of the revision (e.g. "Git", "Local History",
 * "SVN").
 * @param revision The short revision hash, commit ID, or local history label.
 * @param author The author name or committer identity.
 * @param message The commit message or revision note.
 * @author anahata
 */
@Schema(description = "Universal DTO representing a single version control commit or IDE local history entry.")
public record HistoryEntry(
        @Schema(description = "Epoch timestamp in milliseconds.")
        long timestamp,
        @Schema(description = "Human-readable formatted date string.")
        String dateFormatted,
        @Schema(description = "The origin of the revision (e.g. 'Git', 'Local History', 'SVN').")
        String source,
        @Schema(description = "The short revision hash, commit ID, or local history label.")
        String revision,
        @Schema(description = "The author name or committer identity.")
        String author,
        @Schema(description = "The commit message or revision note.")
        String message
        ) implements Comparable<HistoryEntry>, Serializable {

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Sorts history entries in reverse chronological
     * order (newest first).
     * </p>
     */
    @Override
    public int compareTo(HistoryEntry other) {
        return Long.compare(other.timestamp(), this.timestamp());
    }

    /**
     * Formats a list of history entries as a concise Markdown table suitable
     * for RAG context.
     *
     * @param fileName The simple name of the file being documented.
     * @param entries The list of history entries to format.
     * @return Formatted Markdown table string, or null if entries is empty.
     */
    public static String toMarkdownTable(String fileName, List<HistoryEntry> entries) {
        if (entries == null || entries.isEmpty()) {
            return null;
        }
        StringBuilder sb = new StringBuilder();
        sb.append("| Date | Source | Revision / Label | Author | Message |\n");
        sb.append("|---|---|---|---|---|\n");
        for (HistoryEntry e : entries) {
            String dateDisplay = (e.timestamp() > 0)
                    ? TimeUtils.formatSmartTimestamp(Instant.ofEpochMilli(e.timestamp()))
                    : e.dateFormatted();
            String msg = (e.message() != null) ? e.message().replace("\n", " ").trim() : "";
            sb.append("| ").append(dateDisplay)
                    .append(" | ").append(e.source())
                    .append(" | `").append(e.revision()).append("`")
                    .append(" | ").append(e.author() != null ? e.author() : "-")
                    .append(" | ").append(msg)
                    .append(" |\n");
        }
        return sb.toString().trim();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Formats a single history entry as a concise line for tool responses and
     * logs.
     * </p>
     */
    @Override
    public String toString() {
        return "[" + dateFormatted + "] " + source + " " + revision + (author != null && !author.isBlank() ? " by " + author : "") + ": " + (message != null ? message : "");
    }
}
