package uno.anahata.ai.model.resource;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Validate;

/**
 * A powerful, self-contained engine that manages the view of a text-based resource.
 * It holds both the viewport settings (pagination, filtering) and the results
 * of the last processing operation, serving as the definitive source for the
 * current view of a resource. This version uses a character-first pagination model.
 *
 * @author anahata-ai
 */
@Getter
@Setter
@NoArgsConstructor
@Schema(description = "Represents the view port of a chunk of text")
public class TextViewport {

    //<editor-fold defaultstate="collapsed" desc="View Settings">
    private long startChar = 0;
    private int pageSizeInChars = 16 * 1024; // 16KB default page size
    private int columnWidth = 256;
    private String grepPattern;
    private boolean includeLineNumbers = false;
    //</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Cached Process Results">
    @JsonIgnore
    private String processedText;
    private long totalChars;
    private int totalLines;
    private Integer matchingLineCount;
    private int truncatedLinesCount;
    //</editor-fold>

    /**
     * Processes a block of text according to the viewport's settings, caching the results.
     *
     * @param fullText The full text content to process.
     */
    public void process(String fullText) {
        Validate.notNull(fullText, "fullText cannot be null");
        this.totalChars = fullText.length();
        this.totalLines = (int) fullText.lines().count();

        if (fullText.isEmpty()) {
            this.matchingLineCount = 0;
            this.truncatedLinesCount = 0;
            this.processedText = "";
            return;
        }

        // Step 1: Apply grep pattern if it exists
        String contentToPaginate;
        if (grepPattern != null && !grepPattern.trim().isEmpty()) {
            Pattern pattern = Pattern.compile(grepPattern);
            contentToPaginate = fullText.lines()
                    .filter(line -> pattern.matcher(line).matches())
                    .collect(Collectors.joining("\n"));
            this.matchingLineCount = (int) contentToPaginate.lines().count();
        } else {
            contentToPaginate = fullText;
            this.matchingLineCount = null; // Not applicable
        }

        // Step 2: Paginate by character offsets
        long effectiveStart = Math.max(0, startChar);
        long effectiveEnd = Math.min(contentToPaginate.length(), effectiveStart + pageSizeInChars);
        
        if (effectiveStart >= contentToPaginate.length()) {
            this.processedText = "";
            return;
        }

        String pageText = contentToPaginate.substring((int)effectiveStart, (int)effectiveEnd);
        
        // Step 3: Handle partial lines at boundaries
        StringBuilder sb = new StringBuilder();
        
        // Check if the start of our slice is not the start of a line
        if (effectiveStart > 0 && contentToPaginate.charAt((int)effectiveStart - 1) != '\n') {
            int prevNewline = contentToPaginate.lastIndexOf('\n', (int)effectiveStart - 1);
            long precedingChars = effectiveStart - (prevNewline + 1);
            sb.append("[..." + precedingChars + " preceding chars] ");
        }
        
        sb.append(pageText);
        
        // Check if the end of our slice is not the end of a line
        if (effectiveEnd < contentToPaginate.length() && contentToPaginate.charAt((int)effectiveEnd) != '\n') {
            int nextNewline = contentToPaginate.indexOf('\n', (int)effectiveEnd);
            if (nextNewline != -1) {
                long moreChars = nextNewline - effectiveEnd;
                sb.append(" [" + moreChars + " more chars...]");
            }
        }
        
        // Step 4: Truncate long lines and optionally add line numbers
        List<String> pageLines = sb.toString().lines().collect(Collectors.toList());
        this.truncatedLinesCount = (int) pageLines.stream()
                .filter(line -> line.length() > columnWidth)
                .count();

        if (includeLineNumbers) {
            // Line numbers are tricky with char offsets, so we find the starting line number
            int startLineNum = (int) fullText.substring(0, (int)effectiveStart).lines().count();
            final int[] lineNumber = {startLineNum};
            this.processedText = pageLines.stream()
                .map(line -> String.format("[%d]: %s", lineNumber[0]++, truncateLine(line)))
                .collect(Collectors.joining("\n"));
        } else {
            this.processedText = pageLines.stream()
                .map(this::truncateLine)
                .collect(Collectors.joining("\n"));
        }
    }

    private String truncateLine(String line) {
        if (columnWidth > 0 && line.length() > columnWidth) {
            int originalLength = line.length();
            String tag = " ... [truncated " + (originalLength - columnWidth) + " chars] ... ";
            return StringUtils.abbreviateMiddle(line, tag, columnWidth);
        }
        return line;
    }
}
