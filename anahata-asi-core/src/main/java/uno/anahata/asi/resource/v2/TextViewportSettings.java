/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.resource.v2;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * Encapsulates adjustable viewport configuration for V2 text resources.
 */
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Adjustable settings for the text viewport")
public class TextViewportSettings {

    /** The starting character offset for pagination. */
    @Builder.Default
    private long startChar = 0;

    /** The maximum number of characters to load in a single page. */
    @Builder.Default
    private int pageSizeInChars = 64 * 1024;

    /** The maximum line width before horizontal truncation. */
    @Builder.Default
    private int columnWidth = 1024;

    /** A regular expression pattern for filtering lines (grep). */
    private String grepPattern;

    /** Whether to include line numbers in the processed output. */
    @Builder.Default
    private boolean includeLineNumbers = true;

    /** Whether to enable tailing mode. */
    @Builder.Default
    private boolean tail = false;

    /** The number of lines to capture from the end of the source. */
    @Builder.Default
    private int tailLines = 100;

    /**
     * Returns a summary of the settings for use in resource headers.
     * @return A summary string.
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (grepPattern != null && !grepPattern.isBlank()) {
            sb.append("Grep: '").append(grepPattern).append("' ");
        }
        if (tail) {
            sb.append("Tail: ").append(tailLines).append(" lines ");
        } else {
            sb.append(String.format("Range: %d-%d ", startChar, startChar + pageSizeInChars));
        }
        if (includeLineNumbers) {
            sb.append("(+Lines) ");
        }
        if (columnWidth != 1024) {
            sb.append("Cols: ").append(columnWidth).append(" ");
        }
        return sb.length() > 0 ? sb.toString().trim() : "Full View";
    }
}
