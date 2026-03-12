/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.resource.view;

import uno.anahata.asi.agi.resource.handle.ResourceHandle;
import uno.anahata.asi.agi.resource.handle.PathHandle;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.input.ReversedLinesFileReader;
import org.apache.commons.lang3.StringUtils;

/**
 * The V2 Universal Streaming Viewport Engine.
 * <p>
 * Handles memory-efficient processing (Tail, Grep, Pagination) for any 
 * {@link ResourceHandle}. It ensures that huge resources never kill the JVM 
 * heap by streaming content directly from the source.
 * </p>
 * <p>
 * <b>Virtual Fidelity:</b> For virtual resources (snippets), this engine 
 * skips viewport processing and returns the full content to ensure 
 * consistent IDE fidelity.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@Getter
@Setter
@NoArgsConstructor
public class TextViewport {

    /** Current viewport configuration. */
    private TextViewportSettings settings = new TextViewportSettings();

    /** The processed text chunk captured during the last process pass. */
    private String visibleContent;

    /** Total size of the source in characters. */
    private long totalChars;
    
    /** Total lines in the source, or -1 if unknown/large. */
    private int totalLines;
    
    /** Number of matches found if grepping. */
    private Integer matchingLineCount;
    
    /** Number of lines that were horizontally truncated. */
    private int truncatedLinesCount;

    /**
     * Constructs a viewport engine with specific initial settings.
     * @param settings The viewport configuration.
     */
    public TextViewport(TextViewportSettings settings) {
        this.settings = settings;
    }

    /**
     * Processes a resource handle and authoritatively updates the internal 
     * {@code visibleContent} and metrics.
     * 
     * @param handle The source handle.
     * @throws Exception if processing fails.
     */
    public void process(ResourceHandle handle) throws Exception {
        log.debug("Processing viewport engine for: {}", handle.getUri());
        
        // 1. Initial metadata update (if physical and small)
        if (!handle.isVirtual() && handle instanceof PathHandle ph) {
            java.io.File file = new java.io.File(ph.getPath());
            this.totalChars = file.length();
            this.totalLines = (totalChars < 1024 * 1024) ? (int) java.nio.file.Files.lines(file.toPath(), handle.getCharset()).count() : -1;
        } else {
            this.totalChars = -1;
            this.totalLines = -1;
        }

        List<String> lines;
        if (settings.isTail()) {
            lines = processTail(handle);
        } else if (settings.getGrepPattern() != null && !settings.getGrepPattern().isBlank()) {
            lines = processGrep(handle);
        } else {
            lines = processPagination(handle);
        }

        this.visibleContent = finalizeOutput(lines);
    }

    /** 
     * Memory-efficient tail implementation. 
     * @param handle The source handle.
     * @return The list of trailing lines.
     * @throws Exception if reading fails.
     */
    private List<String> processTail(ResourceHandle handle) throws Exception {
        if (!handle.isVirtual() && handle instanceof PathHandle ph) {
            // High-performance backward read for local files
            List<String> lines = new ArrayList<>();
            Pattern pattern = (settings.getGrepPattern() != null) ? Pattern.compile(settings.getGrepPattern()) : null;
            try (ReversedLinesFileReader reader = new ReversedLinesFileReader(new java.io.File(ph.getPath()), handle.getCharset())) {
                String line;
                while ((line = reader.readLine()) != null && lines.size() < settings.getTailLines()) {
                    if (pattern == null || pattern.matcher(line).find()) {
                        lines.add(line);
                    }
                }
            }
            Collections.reverse(lines);
            this.matchingLineCount = (pattern != null) ? lines.size() : null;
            return lines;
        } else {
            // Forward-buffering tail for remote streams
            LinkedList<String> buffer = new LinkedList<>();
            Pattern pattern = (settings.getGrepPattern() != null) ? Pattern.compile(settings.getGrepPattern()) : null;
            int matched = 0;
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(handle.openStream(), handle.getCharset()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    if (pattern == null || pattern.matcher(line).find()) {
                        matched++;
                        buffer.add(line);
                        if (buffer.size() > settings.getTailLines()) {
                            buffer.removeFirst();
                        }
                    }
                }
            }
            this.matchingLineCount = (pattern != null) ? matched : null;
            return new ArrayList<>(buffer);
        }
    }

    /** 
     * Memory-efficient grep implementation. 
     * @param handle The source handle.
     * @return The list of matching lines.
     * @throws Exception if reading fails.
     */
    private List<String> processGrep(ResourceHandle handle) throws Exception {
        List<String> lines = new ArrayList<>();
        Pattern pattern = Pattern.compile(settings.getGrepPattern());
        int matched = 0;
        int maxResults = 500;
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(handle.openStream(), handle.getCharset()))) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (pattern.matcher(line).find()) {
                    matched++;
                    if (lines.size() < maxResults) {
                        lines.add(line);
                    }
                }
            }
        }
        this.matchingLineCount = matched;
        return lines;
    }

    /** 
     * Character-based pagination. 
     * @param handle The source handle.
     * @return The list of lines in the page.
     * @throws Exception if reading fails.
     */
    private List<String> processPagination(ResourceHandle handle) throws Exception {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(handle.openStream(), handle.getCharset()))) {
            reader.skip(settings.getStartChar());
            char[] buffer = new char[settings.getPageSizeInChars()];
            int read = reader.read(buffer);
            if (read <= 0) return Collections.emptyList();
            String chunk = new String(buffer, 0, read);
            return chunk.lines().collect(Collectors.toList());
        }
    }

    /** 
     * Finalizes output with line numbers and truncation. 
     * @param lines The raw processed lines.
     * @return The formatted output string.
     */
    private String finalizeOutput(List<String> lines) {
        this.truncatedLinesCount = 0;
        List<String> processed = new ArrayList<>();
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            
            // 1. Truncation
            if (line.length() > settings.getColumnWidth()) {
                this.truncatedLinesCount++;
                line = StringUtils.abbreviateMiddle(line, " ... [truncated] ... ", settings.getColumnWidth());
            }
            
            // 2. Line Numbers
            if (settings.isIncludeLineNumbers()) {
                processed.add(String.format("%4d | %s", i + 1, line));
            } else {
                processed.add(line);
            }
        }
        return String.join("\n", processed);
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns a descriptive string representing the current state of the viewport engine.</p>
     */
    @Override
    public String toString() {
        return "TextViewport{" + "settings=" + settings + ", totalChars=" + totalChars + ", totalLines=" + totalLines + ", matchingLineCount=" + matchingLineCount + ", truncatedLinesCount=" + truncatedLinesCount + '}';
    }
}
