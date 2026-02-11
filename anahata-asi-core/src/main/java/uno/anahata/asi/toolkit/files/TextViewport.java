package uno.anahata.asi.toolkit.files;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
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

        long start;
        long end;

        if (settings.isTail()) {
            contentToPaginate = getTail(contentToPaginate, settings.getTailLines());
            start = 0;
            end = Math.min(contentToPaginate.length(), settings.getPageSizeInChars());
        } else {
            start = Math.max(0, settings.getStartChar());
            end = Math.min(contentToPaginate.length(), start + settings.getPageSizeInChars());
        }
        
        if (start >= contentToPaginate.length()) {
            this.processedText = "";
            return;
        }

        String pageText = contentToPaginate.substring((int)start, (int)end);
        StringBuilder sb = new StringBuilder();
        
        if (!settings.isTail() && start > 0 && contentToPaginate.charAt((int)start - 1) != '\n') {
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
            int startLineNum;
            if (settings.isTail()) {
                // For tail, we need to calculate the line number offset in the original text
                // This is a bit complex if grep is active, but for now we'll just count lines in fullText
                // until we reach the start of contentToPaginate.
                // Simplified: just count lines in fullText before the tail.
                // But wait, contentToPaginate might be filtered.
                // Let's just use the line count from the start of the tail in the current contentToPaginate.
                startLineNum = (int) fullText.substring(0, fullText.indexOf(pageText)).lines().count();
            } else {
                startLineNum = (int) fullText.substring(0, (int)start).lines().count();
            }
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

    private String getTail(String text, int n) {
        if (n <= 0) return "";
        int count = 0;
        int pos = text.length();
        while (count < n && pos > 0) {
            pos = text.lastIndexOf('\n', pos - 1);
            if (pos == -1) {
                pos = 0;
                break;
            }
            count++;
        }
        return (pos > 0) ? text.substring(pos + 1) : text;
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

    @Override
    public String toString() {
        return "TextViewport{" + "settings=" + settings + ", totalChars=" + totalChars + ", totalLines=" + totalLines + ", matchingLineCount=" + matchingLineCount + ", truncatedLinesCount=" + truncatedLinesCount + '}';
    }
}
