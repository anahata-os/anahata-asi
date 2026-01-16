package uno.anahata.asi.model.resource;

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
 * A powerful engine that manages the view of a text resource.
 * Holds process results and a nested settings object.
 * 
 * @author anahata-ai
 */
@Getter
@Setter
@NoArgsConstructor
@Schema(description = "Represents the view port of a chunk of text")
public class TextViewport {

    @Schema(description = "Adjustable viewport configuration")
    private TextViewportSettings settings = new TextViewportSettings();

    @JsonIgnore
    private String processedText;
    private long totalChars;
    private int totalLines;
    private Integer matchingLineCount;
    private int truncatedLinesCount;

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

        String contentToPaginate = fullText;
        if (settings.getGrepPattern() != null && !settings.getGrepPattern().trim().isEmpty()) {
            Pattern pattern = Pattern.compile(settings.getGrepPattern());
            contentToPaginate = fullText.lines()
                    .filter(line -> pattern.matcher(line).matches())
                    .collect(Collectors.joining("\n"));
            this.matchingLineCount = (int) contentToPaginate.lines().count();
        } else {
            this.matchingLineCount = null;
        }

        long start = Math.max(0, settings.getStartChar());
        long end = Math.min(contentToPaginate.length(), start + settings.getPageSizeInChars());
        
        if (start >= contentToPaginate.length()) {
            this.processedText = "";
            return;
        }

        String pageText = contentToPaginate.substring((int)start, (int)end);
        StringBuilder sb = new StringBuilder();
        
        if (start > 0 && contentToPaginate.charAt((int)start - 1) != '\n') {
            int prevNewline = contentToPaginate.lastIndexOf('\n', (int)start - 1);
            sb.append("[..." + (start - (prevNewline + 1)) + " preceding chars] ");
        }
        
        sb.append(pageText);
        
        if (end < contentToPaginate.length() && contentToPaginate.charAt((int)end) != '\n') {
            int nextNewline = contentToPaginate.indexOf('\n', (int)end);
            if (nextNewline != -1) sb.append(" [" + (nextNewline - end) + " more chars...]");
        }
        
        List<String> pageLines = sb.toString().lines().collect(Collectors.toList());
        this.truncatedLinesCount = (int) pageLines.stream()
                .filter(line -> line.length() > settings.getColumnWidth())
                .count();

        if (settings.isShowLineNumbers()) {
            int startLineNum = (int) fullText.substring(0, (int)start).lines().count();
            final int[] ln = {startLineNum + 1};
            this.processedText = pageLines.stream()
                .map(line -> String.format("[%d]: %s", ln[0]++, truncateLine(line)))
                .collect(Collectors.joining("\n"));
        } else {
            this.processedText = pageLines.stream()
                .map(this::truncateLine)
                .collect(Collectors.joining("\n"));
        }
    }

    /**
     * Truncates a single line of text if it exceeds the configured column width.
     * 
     * @param line The line to truncate.
     * @return The truncated line.
     */
    private String truncateLine(String line) {
        int width = settings.getColumnWidth();
        if (width > 0 && line.length() > width) {
            String tag = " ... [truncated " + (line.length() - width) + " chars] ... ";
            return StringUtils.abbreviateMiddle(line, tag, width);
        }
        return line;
    }
    
    /**
     * Returns a human-readable string representation of the viewport's state.
     * 
     * @return The formatted string.
     */
    @Override
    public String toString() {
        return String.format("Viewport[start=%d, size=%d, lines=%d/%d, truncated=%d, lineNumbers=%b]", 
            settings.getStartChar(), settings.getPageSizeInChars(), 
            matchingLineCount != null ? matchingLineCount : totalLines, 
            totalLines, truncatedLinesCount, settings.isShowLineNumbers());
    }
}
