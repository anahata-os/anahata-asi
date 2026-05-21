package uno.anahata.asi.internal;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;

/**
 * Utility class designed to generate concise, human-readable summaries and truncations 
 * of complex Java objects, collections, maps, and strings.
 *
 * @author anahata
 */
public class ObjectSummarizer {

    /**
     * Checks if an object is null, a blank string, or an empty collection/map.
     * @param value The object to check.
     * @return true if the object is considered null or empty.
     */
    public static boolean isNullOrEmpty(Object value) {
        if (value == null) {
            return true;
        }
        if (value instanceof String && ((String) value).isBlank()) {
            return true;
        }
        if (value instanceof Collection && ((Collection<?>) value).isEmpty()) {
            return true;
        }
        if (value instanceof Map && ((Map<?, ?>) value).isEmpty()) {
            return true;
        }
        return false;
    }

    /**
     * Formats a value for display in a summary. It intelligently handles collections, maps, and long strings.
     * @param value The object to format.
     * @param maxLength The maximum length for the formatted string or individual lines.
     * @return A formatted, summarized, and truncated string.
     */
    public static String formatValue(Object value, int maxLength) {
        return formatValue(value, maxLength, true);
    }
    
    /**
     * Formats a value for display in a summary with fine-grained control over map summarization.
     * @param value The object to format.
     * @param maxLength The maximum length for the formatted string or individual lines.
     * @param summarizeMapValues If true, map values are recursively summarized; otherwise, a simple entry count is shown.
     * @return A formatted, summarized, and truncated string.
     */
    public static String formatValue(Object value, int maxLength, boolean summarizeMapValues) {
        if (value == null) {
            return "null";
        }
        
        if (value instanceof Map) {
            if (summarizeMapValues) {
                return formatMap((Map<?, ?>) value, maxLength);
            } else {
                return "{" + ((Map<?, ?>) value).size() + " entries}";
            }
        }
        
        if (value instanceof Collection) {
            return "[" + ((Collection<?>) value).size() + " items]";
        }

        // Normalize line endings by removing carriage returns for consistent processing.
        String s = String.valueOf(value).replace("\r", "");
        
        // First, truncate individual long lines to prevent them from dominating the summary
        String lineProcessedString = Arrays.stream(s.split("\n"))
                                           .map(line -> truncateLine(line, maxLength))
                                           .collect(Collectors.joining("\\n"));

        // Then, truncate the overall string if it's still too long
        int totalChars = lineProcessedString.length();
        if (totalChars <= maxLength) {
            return lineProcessedString;
        }
        
        String middle = " ... [truncated " + totalChars + " chars] ... ";
        return StringUtils.abbreviateMiddle(lineProcessedString, middle, maxLength);
    }
    
    /**
     * Formats and recursively summarizes the entries of a Map.
     *
     * @param map The map to format.
     * @param maxLength The maximum allowed length of the resulting string.
     * @return A serialized and summarized representation of the map.
     */
    private static String formatMap(Map<?, ?> map, int maxLength) {
        String summary = map.entrySet().stream()
            .map(entry -> entry.getKey() + "=" + formatValue(entry.getValue(), maxLength, false)) // Recursively call, but don't re-summarize sub-maps
            .collect(Collectors.joining(", ", "{", "}"));
        
        if (summary.length() <= maxLength) {
            return summary;
        }
        
        String middle = " ... [truncated " + summary.length() + " chars] ... ";
        return StringUtils.abbreviateMiddle(summary, middle, maxLength);
    }

    /**
     * Truncates an individual string line to prevent it from exceeding a maximum length threshold.
     *
     * @param line The line of text to truncate.
     * @param maxLineLength The maximum allowed length for the line.
     * @return The truncated line, or the original line if it fits within the limit.
     */
    private static String truncateLine(String line, int maxLineLength) {
        if (maxLineLength > 0 && line.length() > maxLineLength) {
            int originalLength = line.length();
            String tag = " ... [truncated " + originalLength + " chars] ... ";
            return StringUtils.abbreviateMiddle(line, tag, maxLineLength);
        }
        return line;
    }
}
