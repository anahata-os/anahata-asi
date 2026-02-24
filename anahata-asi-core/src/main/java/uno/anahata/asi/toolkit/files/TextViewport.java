package uno.anahata.asi.toolkit.files;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import java.io.BufferedReader;
import java.io.File;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.input.ReversedLinesFileReader;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Validate;

/**
 * A powerful engine that manages the view of a text resource.
 * It supports both in-memory processing for small strings and 
 * on-disk streaming for large files to prevent memory bloat.
 * 
 * @author anahata-ai
 */
@Getter
@Setter
@NoArgsConstructor
@Schema(description = "Represents the view port of a chunk of text")
@Slf4j
public class TextViewport {

    @Schema(description = "Adjustable viewport configuration")
    private TextViewportSettings settings = new TextViewportSettings();

    @JsonIgnore    
    private String processedText;
    private long totalChars;
    private int totalLines;
    private Integer matchingLineCount;
    private int truncatedLinesCount;

    /**
     * Processes the viewport from a Path using streaming to save memory.
     * This method correctly sets the total characters and lines for the entire file.
     * 
     * @param path The path to the file.
     * @param charset The charset to use for reading.
     * @throws Exception if an I/O error occurs.
     */
    public void process(Path path, Charset charset) throws Exception {
        File file = path.toFile();
        this.totalChars = file.length();
        
        // Use a heuristic for total lines to avoid full-file scan on every reload
        // unless the file is small (< 1MB)
        if (totalChars < 1024 * 1024) {
            try (var lines = Files.lines(path, charset)) {
                this.totalLines = (int) lines.count();
            }
        } else {
            this.totalLines = -1; // Indicates "Unknown/Large"
        }

        if (settings.isTail()) {
            processTailStreaming(file, charset);
        } else if (settings.getGrepPattern() != null && !settings.getGrepPattern().isBlank()) {
            processGrepStreaming(path, charset);
        } else {
            processDirectPaging(path, charset);
        }
    }

    /**
     * Performs a memory-efficient tail operation by reading the file backwards from disk.
     * If a grep pattern is set, it matches lines as it reads them until the target
     * line count is reached.
     * 
     * @param file The file to read.
     * @param charset The encoding of the file.
     * @throws Exception if an I/O error occurs.
     */
    private void processTailStreaming(File file, Charset charset) throws Exception {
        List<String> lines = new ArrayList<>();
        Pattern pattern = (settings.getGrepPattern() != null && !settings.getGrepPattern().isBlank()) 
                ? Pattern.compile(settings.getGrepPattern()) : null;
        
        int targetCount = settings.getTailLines();
        
        try (ReversedLinesFileReader reader = new ReversedLinesFileReader(file, charset)) {
            String line;
            while ((line = reader.readLine()) != null && lines.size() < targetCount) {
                if (pattern == null || pattern.matcher(line).find()) {
                    lines.add(line);
                }
            }
        }
        Collections.reverse(lines);
        finalizeLines(lines, pattern != null ? lines.size() : null);
    }

    /**
     * Performs a memory-efficient forward grep by streaming the file line-by-line.
     * This avoids loading the entire file content if only a subset of lines match.
     * 
     * @param path The path to the file.
     * @param charset The encoding of the file.
     * @throws Exception if an I/O error occurs.
     */
    private void processGrepStreaming(Path path, Charset charset) throws Exception {
        List<String> lines = new ArrayList<>();
        Pattern pattern = Pattern.compile(settings.getGrepPattern());
        
        // For forward grep, we still need to limit memory
        int maxLinesToReturn = 500; 
        int matched = 0;
        
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (pattern.matcher(line).find()) {
                    matched++;
                    if (lines.size() < maxLinesToReturn) {
                        lines.add(line);
                    }
                }
            }
        }
        this.matchingLineCount = matched;
        finalizeLines(lines, matched);
    }

    /**
     * Reads a specific range of characters from the file directly into memory.
     * This is used for simple pagination without filters.
     * 
     * @param path The path to the file.
     * @param charset The encoding of the file.
     * @throws Exception if an I/O error occurs.
     */
    private void processDirectPaging(Path path, Charset charset) throws Exception {
        // For simple paging, we only read the window
        long start = Math.max(0, settings.getStartChar());
        int size = settings.getPageSizeInChars();
        
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            reader.skip(start);
            char[] buffer = new char[size];
            int read = reader.read(buffer);
            if (read > 0) {
                // Internal call to process the read chunk without overwriting global totals
                processContent(new String(buffer, 0, read), false);
            } else {
                this.processedText = "";
            }
        }
    }

    /**
     * Legacy/In-memory processor for small strings or full content loads.
     * This updates the global metadata based on the provided string.
     * 
     * @param fullText The full text string to process.
     */
    public void process(String fullText) {
        Validate.notNull(fullText, "fullText cannot be null");
        processContent(fullText, true);
    }

    /**
     * Internal logic for processing content, with optional metadata update.
     * 
     * @param text The text to process.
     * @param updateTotals Whether to update totalChars and totalLines based on this text.
     */
    private void processContent(String text, boolean updateTotals) {
        if (updateTotals) {
            this.totalChars = text.length();
            this.totalLines = (int) text.lines().count();
        }

        String contentToPaginate = text;
        if (settings.getGrepPattern() != null && !settings.getGrepPattern().trim().isEmpty()) {
            Pattern pattern = Pattern.compile(settings.getGrepPattern());
            contentToPaginate = text.lines()
                    .filter(line -> pattern.matcher(line).find())
                    .collect(Collectors.joining("\n"));
            this.matchingLineCount = (int) contentToPaginate.lines().count();
        } else {
            this.matchingLineCount = null;
        }

        if (settings.isTail()) {
            contentToPaginate = getTail(contentToPaginate, settings.getTailLines());
        }
        
        finalizeLines(contentToPaginate.lines().collect(Collectors.toList()), matchingLineCount);
    }

    /**
     * Updates internal state with processed lines and handles line-level truncation.
     * 
     * @param lines The final list of lines to display.
     * @param matchCount Total occurrences found (if filtering).
     */
    private void finalizeLines(List<String> lines, Integer matchCount) {
        this.matchingLineCount = matchCount;
        this.truncatedLinesCount = (int) lines.stream()
                .filter(l -> l.length() > settings.getColumnWidth())
                .count();

        this.processedText = lines.stream()
                .map(this::truncateLine)
                .collect(Collectors.joining("\n"));
    }

    /**
     * Extracts the last N lines from a string.
     * 
     * @param text The input string.
     * @param n Maximum lines to return.
     * @return The tailing lines.
     */
    private String getTail(String text, int n) {
        if (n <= 0) return "";
        List<String> all = text.lines().collect(Collectors.toList());
        int start = Math.max(0, all.size() - n);
        return all.subList(start, all.size()).stream().collect(Collectors.joining("\n"));
    }

    /**
     * Truncates a single line to the configured column width.
     * 
     * @param line The input line.
     * @return The truncated line with an abbreviation marker.
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
        return "TextViewport{" + "settings=" + settings + ", totalChars=" + totalChars + ", totalLines=" + (totalLines == -1 ? "Large/Unknown" : totalLines) + ", matchingLineCount=" + matchingLineCount + ", truncatedLinesCount=" + truncatedLinesCount + '}';
    }
}
