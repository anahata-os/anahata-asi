/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.internal;

import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import lombok.experimental.UtilityClass;
import org.apache.commons.lang3.StringUtils;

/**
 * A collection of general-purpose text and formatting utility methods.
 *
 * @author pablo
 */
@UtilityClass
public class TextUtils {

    /**
     * Formats a byte size into a human-readable string (e.g., "1.2 KB", "5.5 MB").
     *
     * @param size The size in bytes.
     * @return The formatted string.
     */
    public static String formatSize(long size) {
        if (size < 1024) {
            return size + " B";
        }
        int exp = (int) (Math.log(size) / Math.log(1024));
        char pre = "KMGTPE".charAt(exp - 1);
        return String.format("%.1f %sB", size / Math.pow(1024, exp), pre);
    }

    /**
     * Checks if an object is null, a blank string, or an empty collection/map.
     *
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
     * Formats a value for display in a summary, escaping newlines and
     * truncating in the middle if necessary.
     *
     * @param value The object to format.
     * @return A formatted, truncated string.
     */
    public static String formatValue(Object value) {
        if (value == null) {
            return "null";
        }
        if (value instanceof byte[] bytes) {
            return formatByteArray(bytes);
        }
        int maxLength = 64;
        String s = String.valueOf(value).replace("\n", "\\n").replace("\r", "");
        int totalChars = s.length();
        String tag = " *(..." + (totalChars - maxLength) + " more chars...)* ";
        if (totalChars > (maxLength + tag.length() + 8)) {
            return StringUtils.abbreviateMiddle(s, tag, maxLength);
        } else {
            return s;
        }
    }

    /**
     * Formats a byte array into a concise string representation, showing the
     * first and last three bytes in 0-255 format and truncating the middle.
     *
     * @param data The byte array to format.
     * @return A formatted string (e.g., "[12, 34, 56 ... 78, 90, 12]").
     */
    public static String formatByteArray(byte[] data) {
        if (data == null) {
            return "null";
        }
        if (data.length <= 6) {
            return IntStream.range(0, data.length)
                    .mapToObj(i -> String.valueOf(data[i] & 0xFF))
                    .collect(Collectors.joining(", ", "[", "]"));
        } else {
            String firstThree = IntStream.range(0, 3)
                    .mapToObj(i -> String.valueOf(data[i] & 0xFF))
                    .collect(Collectors.joining(", "));
            String lastThree = IntStream.range(data.length - 3, data.length)
                    .mapToObj(i -> String.valueOf(data[i] & 0xFF))
                    .collect(Collectors.joining(", "));
            return String.format("[%s ... %s]", firstThree, lastThree);
        }
    }
}
