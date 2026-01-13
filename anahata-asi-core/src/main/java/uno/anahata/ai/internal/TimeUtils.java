package uno.anahata.ai.internal;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.TimeUnit;
import org.apache.commons.lang3.time.DurationFormatUtils;

/**
 * Utility class for time-related formatting.
 * @author AI
 */
public class TimeUtils {

    private static final DateTimeFormatter TIME_FORMATTER = DateTimeFormatter.ofPattern("HH:mm:ss");
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("MMM dd");

    /**
     * Formats a duration in milliseconds into a HH:MM:SS string.
     *
     * @param millis The duration in milliseconds.
     * @return A string formatted as HH:MM:SS.
     */
    public static String formatMillis(long millis) {
        long hours = TimeUnit.MILLISECONDS.toHours(millis);
        long minutes = TimeUnit.MILLISECONDS.toMinutes(millis) % 60;
        long seconds = TimeUnit.MILLISECONDS.toSeconds(millis) % 60;
        return String.format("%02d:%02d:%02d", hours, minutes, seconds);
    }

    /**
     * Formats a duration in milliseconds into a concise string like "1h 2m 3s".
     * Omits zero-value components.
     *
     * @param millis The duration in milliseconds.
     * @return A concise, human-readable time string.
     */
    public static String formatMillisConcise(long millis) {
        if (millis < 1000) {
            return "0s";
        }
        // Use Apache Commons Lang to create a format like "1 hour 2 minutes 3 seconds"
        // and then abbreviate it.
        String formatted = DurationFormatUtils.formatDurationWords(millis, true, true);
        return formatted
                .replace(" hours", "h")
                .replace(" hour", "h")
                .replace(" minutes", "m")
                .replace(" minute", "m")
                .replace(" seconds", "s")
                .replace(" second", "s");
    }

    /**
     * Formats a duration in milliseconds into a human-readable string (e.g., "120ms", "1.52s", "1m 32s").
     * @param millis The duration in milliseconds.
     * @return A formatted string.
     */
    public static String formatDuration(long millis) {
        if (millis < 1000) {
            return millis + "ms";
        }
        if (millis < 60000) {
            return String.format("%.2fs", millis / 1000.0);
        }
        long minutes = millis / 60000;
        long seconds = (millis % 60000) / 1000;
        return String.format("%dm %ds", minutes, seconds);
    }

    /**
     * Formats an Instant into a compact, smart string. It shows only the time for today's dates
     * and a short date format for all other dates.
     * @param timestamp The Instant to format.
     * @return A compact, formatted date/time string.
     */
    public static String formatSmartTimestamp(Instant timestamp) {
        if (timestamp == null) {
            return "N/A";
        }
        LocalDate messageDate = timestamp.atZone(ZoneId.systemDefault()).toLocalDate();
        LocalDate today = LocalDate.now(ZoneId.systemDefault());

        if (messageDate.equals(today)) {
            return TIME_FORMATTER.format(timestamp.atZone(ZoneId.systemDefault()));
        } else {
            return DATE_FORMATTER.format(timestamp.atZone(ZoneId.systemDefault()));
        }
    }

    /**
     * Calculates and formats the elapsed time between two Instants.
     * @param start The start time.
     * @param end The end time.
     * @return A formatted duration string, or "N/A" if either timestamp is null.
     */
    public static String getElapsedString(Instant start, Instant end) {
        if (start == null || end == null) {
            return "N/A";
        }
        long elapsedMillis = Duration.between(start, end).toMillis();
        return formatDuration(elapsedMillis);
    }
}
