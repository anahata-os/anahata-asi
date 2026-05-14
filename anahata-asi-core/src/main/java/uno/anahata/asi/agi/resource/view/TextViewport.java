/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.resource.view;

import uno.anahata.asi.agi.resource.handle.ResourceHandle;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
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
     * Expands the viewport settings to fit the entire resource content.
     * <p>
     * This adjusts the pagination to start from 0 and include the full 
     * resource size. It also expands the column width to the maximum integer 
     * value if any lines were previously truncated, ensuring an unclipped view.
     * </p>
     */
    public void expandToFit() {
        settings.setStartChar(0);
        settings.setPageSizeInChars((int) Math.min(Integer.MAX_VALUE, Math.max(1024L, totalChars)));
        if (truncatedLinesCount > 0) {
            settings.setColumnWidth(Integer.MAX_VALUE);
        }
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
        
        // 1. Initial metadata update
        this.totalChars = handle.length();

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
        if (!handle.isVirtual() && "file".equalsIgnoreCase(handle.getUri().getScheme())) {
            // High-performance backward read for local files
            List<String> lines = new ArrayList<>();
            Pattern pattern = (settings.getGrepPattern() != null) ? Pattern.compile(settings.getGrepPattern()) : null;
            java.io.File file = new java.io.File(handle.getUri());
            
            Charset charset = handle.getCharset();
            // Workaround for commons-io 2.19.0 object identity bug in modular environments
            if ("UTF-8".equalsIgnoreCase(charset.name())) {
                charset = StandardCharsets.UTF_8;
            }

            try (ReversedLinesFileReader reader = ReversedLinesFileReader.builder()
                    .setFile(file)
                    .setCharset(charset)
                    .get()) {
                String line;
                while ((line = reader.readLine()) != null && lines.size() < settings.getTailLines()) {
                    if (pattern == null || pattern.matcher(line).find()) {
                        lines.add(line);
                    }
                }
            } catch (Throwable e) {
                log.warn("ReversedLinesFileReader failed for {} ({}). Falling back to forward-buffering tail. Reason: {}", 
                        handle.getUri(), charset, e.getMessage());
                return processForwardTail(handle);
            }
            Collections.reverse(lines);
            this.matchingLineCount = (pattern != null) ? lines.size() : null;
            return lines;
        } else {
            return processForwardTail(handle);
        }
    }

    /**
     * Fallback forward-buffering tail for remote streams or when backward reading fails.
     * @param handle The source handle.
     * @return The list of trailing lines.
     * @throws Exception if reading fails.
     */
    private List<String> processForwardTail(ResourceHandle handle) throws Exception {
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
            int totalRead = 0;
            int charsRead;
            while (totalRead < buffer.length && (charsRead = reader.read(buffer, totalRead, buffer.length - totalRead)) != -1) {
                totalRead += charsRead;
            }
            if (totalRead <= 0) return Collections.emptyList();
            String chunk = new String(buffer, 0, totalRead);
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
            
            // 1. Truncation (Now Wrapping)
            if (line.length() > settings.getColumnWidth()) {
                this.truncatedLinesCount++;
                int start = 0;
                while (start < line.length()) {
                    int end = Math.min(start + settings.getColumnWidth(), line.length());
                    String chunk = line.substring(start, end);
                    if (settings.isIncludeLineNumbers()) {
                        processed.add(String.format("%4d | %s", i + 1, chunk));
                    } else {
                        processed.add(chunk);
                    }
                    start = end;
                }
            } else {
                // 2. Line Numbers
                if (settings.isIncludeLineNumbers()) {
                    processed.add(String.format("%4d | %s", i + 1, line));
                } else {
                    processed.add(line);
                }
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
        return "TextViewport{" + "settings=" + settings + ", totalChars=" + totalChars + ", matchingLineCount=" + matchingLineCount + ", truncatedLinesCount=" + truncatedLinesCount + '}';
    }
}
