package uno.anahata.asi.toolkit.files;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * Encapsulates adjustable viewport configuration for text resources.
 * 
 * @author anahata-ai
 */
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Schema(description = "Adjustable settings for the text viewport")
public class TextViewportSettings {
    @Builder.Default
    private long startChar = 0;
    @Builder.Default
    private int pageSizeInChars = 64 * 1024;
    @Builder.Default
    private int columnWidth = 1024;
    private String grepPattern;
    @Builder.Default
    private boolean showLineNumbers = false;
    @Builder.Default
    private boolean tail = false;
    @Builder.Default
    private int tailLines = 100;

    /**
     * Returns a comprehensive summary of the viewport settings for UI display.
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
            if (startChar > 0 || pageSizeInChars < Integer.MAX_VALUE) {
                sb.append(String.format("Range: %d-%d ", startChar, startChar + pageSizeInChars));
            }
        }
        if (showLineNumbers) {
            sb.append("(+Lines) ");
        }
        if (columnWidth != 1024) {
            sb.append("Cols: ").append(columnWidth).append(" ");
        }
        
        String res = sb.toString().trim();
        return res.isEmpty() ? "Full View" : res;
    }
}
