/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.context;

import java.util.List;
import java.util.Map;
import uno.anahata.asi.agi.Agi;

/**
 * Standardized context annotation and tooltip formatter for IDE project and file views.
 * <p>
 * Provides consistent formatting across all IDE plugins (NetBeans, IntelliJ, Desktop):
 * </p>
 * <ul>
 *   <li><b>Directories / Packages / Folders:</b> Bracketed resource counts per session (e.g. {@code [4]} or {@code [4][1]}).</li>
 *   <li><b>Files:</b> Single session displays the session nickname in parentheses (e.g. {@code (Intellij)}),
 *       while multiple sessions display the active session count (e.g. {@code (3)}).</li>
 * </ul>
 *
 * @author anahata
 */
public final class ContextAnnotationFormatter {

    /**
     * Non-instantiable utility class.
     */
    private ContextAnnotationFormatter() {
    }

    /**
     * Formats context counts into clean text annotations for IDE labels or location strings.
     *
     * @param sessionCounts Map of active sessions to their respective resource counts.
     * @param isDirectory True if the annotated node represents a directory, package, or container folder.
     * @return The formatted annotation string (e.g. {@code "[4]"} or {@code "(Intellij)"}), or {@code null} if empty.
     */
    public static String formatAnnotation(Map<Agi, Integer> sessionCounts, boolean isDirectory) {
        if (sessionCounts == null || sessionCounts.isEmpty()) {
            return null;
        }

        if (isDirectory) {
            StringBuilder sb = new StringBuilder();
            for (Map.Entry<Agi, Integer> entry : sessionCounts.entrySet()) {
                if (entry.getValue() > 0) {
                    sb.append("[").append(entry.getValue()).append("]");
                }
            }
            return sb.length() > 0 ? sb.toString() : null;
        } else {
            long nonZeroCount = sessionCounts.values().stream().filter(c -> c > 0).count();
            if (nonZeroCount == 1) {
                for (Map.Entry<Agi, Integer> entry : sessionCounts.entrySet()) {
                    if (entry.getValue() > 0) {
                        return "(" + entry.getKey().getDisplayName() + ")";
                    }
                }
            } else if (nonZeroCount > 1) {
                return "(" + nonZeroCount + ")";
            }
            return null;
        }
    }

    /**
     * Formats context presence into clean text annotations from parallel lists of sessions and counts.
     *
     * @param agis The list of active AGI sessions.
     * @param totals The corresponding context presence totals.
     * @param isDirectory True if the node is a directory/folder/package.
     * @return The formatted annotation string, or {@code null} if empty.
     */
    public static String formatAnnotation(List<Agi> agis, List<Integer> totals, boolean isDirectory) {
        if (totals == null || totals.isEmpty()) {
            return null;
        }
        if (isDirectory) {
            StringBuilder sb = new StringBuilder();
            for (Integer sum : totals) {
                if (sum != null && sum > 0) {
                    sb.append("[").append(sum).append("]");
                }
            }
            return sb.length() > 0 ? sb.toString() : null;
        } else {
            long sessionsCount = totals.stream().filter(i -> i != null && i > 0).count();
            if (sessionsCount == 1 && agis != null) {
                for (int i = 0; i < totals.size(); i++) {
                    if (totals.get(i) != null && totals.get(i) > 0 && i < agis.size()) {
                        return "(" + agis.get(i).getDisplayName() + ")";
                    }
                }
            } else if (sessionsCount > 1) {
                return "(" + sessionsCount + ")";
            }
            return null;
        }
    }

    /**
     * Formats context counts into an HTML snippet with standard muted grey color ({@code #707070}).
     *
     * @param sessionCounts Map of active sessions to their respective resource counts.
     * @param isDirectory True if the annotated node represents a directory, package, or container folder.
     * @return The HTML snippet (e.g. {@code " <font color='#707070'>[4]</font>"}), or {@code ""} if empty.
     */
    public static String formatAnnotationHtml(Map<Agi, Integer> sessionCounts, boolean isDirectory) {
        String raw = formatAnnotation(sessionCounts, isDirectory);
        if (raw == null || raw.isBlank()) {
            return "";
        }
        return " <font color='#707070'>" + raw + "</font>";
    }

    /**
     * Formats context presence into an HTML snippet from parallel lists of sessions and counts.
     *
     * @param agis The list of active AGI sessions.
     * @param totals The corresponding context presence totals.
     * @param isDirectory True if the node is a directory/folder/package.
     * @return The HTML snippet, or {@code ""} if empty.
     */
    public static String formatAnnotationHtml(List<Agi> agis, List<Integer> totals, boolean isDirectory) {
        String raw = formatAnnotation(agis, totals, isDirectory);
        if (raw == null || raw.isBlank()) {
            return "";
        }
        return " <font color='#707070'>" + raw + "</font>";
    }

    /**
     * Builds a clean plain-text tooltip describing which sessions hold the resource.
     *
     * @param sessionCounts Map of active sessions to their resource counts.
     * @param isDirectory True if the node represents a directory or folder.
     * @return A multi-line plain text tooltip string, or {@code null} if empty.
     */
    public static String buildTooltipText(Map<Agi, Integer> sessionCounts, boolean isDirectory) {
        if (sessionCounts == null || sessionCounts.isEmpty()) {
            return null;
        }
        StringBuilder sb = new StringBuilder("In context in:");
        boolean hasAny = false;
        for (Map.Entry<Agi, Integer> entry : sessionCounts.entrySet()) {
            if (entry.getValue() > 0) {
                hasAny = true;
                sb.append("\n • ").append(entry.getKey().getDisplayName());
                if (isDirectory) {
                    sb.append(": ").append(entry.getValue()).append(" resource").append(entry.getValue() == 1 ? "" : "s");
                }
            }
        }
        return hasAny ? sb.toString() : null;
    }
}
