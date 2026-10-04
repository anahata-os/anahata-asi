/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.tools.hints;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Universal DTO representing a single code diagnostic, warning, or inspection hint.
 * <p>
 * Contains line and column positions, localized severity string, human-readable
 * description, and the unique rule or inspection identifier assigned by the host IDE.
 * </p>
 *
 * @author anahata
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class HintInfo {

    /**
     * The absolute canonical path of the file containing the hint.
     */
    private String filePath;

    /**
     * A human-readable description or diagnostic message.
     */
    private String description;

    /**
     * The localized severity string as reported by the host IDE
     * (e.g. {@code ERROR}, {@code WARNING}, {@code VERIFY}, {@code HINT}, {@code WEAK_WARNING}).
     */
    private String severity;

    /**
     * The 1-based line number where the diagnostic starts.
     */
    private int line;

    /**
     * The 1-based column number where the diagnostic starts.
     */
    private int column;

    /**
     * The unique identifier or short name of the hint or inspection rule.
     */
    private String id;

    /**
     * Formats this single hint into a Markdown list item.
     *
     * @return Formatted Markdown string for this hint.
     */
    public String toMarkdown() {
        StringBuilder sb = new StringBuilder();
        sb.append("- [").append(severity).append("] line ").append(line);
        if (column > 0) {
            sb.append(", col ").append(column);
        }
        sb.append(": ").append(description);
        if (id != null && !id.isBlank()) {
            sb.append(" (").append(id).append(")");
        }
        return sb.toString();
    }

    /**
     * Formats a list of hints into a Markdown block without repeating the file name.
     *
     * @param hints The list of hints to format.
     * @return A formatted Markdown string, or {@code null} if hints list is empty.
     */
    public static String toMarkdown(List<HintInfo> hints) {
        return toMarkdown(null, hints);
    }

    /**
     * Formats a list of hints into a Markdown block, optionally titled with the file name.
     *
     * @param fileName Optional file name to include in the header, or null/empty to omit.
     * @param hints The list of hints to format.
     * @return A formatted Markdown string, or {@code null} if hints list is empty.
     */
    public static String toMarkdown(String fileName, List<HintInfo> hints) {
        if (hints == null || hints.isEmpty()) {
            return null;
        }
        StringBuilder sb = new StringBuilder();
        if (fileName != null && !fileName.isBlank()) {
            sb.append("### Inspection Hints (`").append(fileName).append("`):\n");
        } else {
            sb.append("### Inspection Hints:\n");
        }
        for (HintInfo h : hints) {
            sb.append(h.toMarkdown()).append("\n");
        }
        return sb.toString();
    }
}
