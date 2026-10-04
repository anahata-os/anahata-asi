/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.resources.text;

/**
 * Shared utility for calculating line numbers and mappings in agentic diff renderers.
 * <p>
 * This utility provides consistent line-counting and indexing logic, ensuring that 
 * AI-generated comments are correctly mapped between original and proposed content 
 * despite cumulative line shifts caused by insertions or deletions.
 * </p>
 * 
 * @author anahata
 */
public class DiffCommentUtils {

    /**
     * Counts the number of lines in a string, ensuring platform-agnostic results.
     * <p>
     * <b>Technical Note:</b> Uses the {@code \\R} regex to detect any Unicode 
     * line sequence and preserves trailing empty lines via the {@code -1} limit.
     * </p>
     * 
     * @param text The text to analyze.
     * @return The number of lines, or 0 if the text is null or empty.
     */
    public static int getLineCount(String text) {
        if (text == null || text.isEmpty()) {
            return 0;
        }
        return text.split("\\R", -1).length;
    }

    /**
     * Identifies the 1-based line number of a character index within a text.
     * Handles LF (\n), CR (\r), and CRLF (\r\n) sequences consistently.
     * 
     * @param text The text to search.
     * @param charIndex The 0-based character index.
     * @return The 1-based line number.
     */
    public static int getLineAt(String text, int charIndex) {
        if (text == null || charIndex <= 0) {
            return 1;
        }
        
        int lineNum = 1;
        int length = Math.min(charIndex, text.length());
        for (int i = 0; i < length; i++) {
            char c = text.charAt(i);
            if (c == '\n') {
                // LF or the end of a CRLF sequence
                lineNum++;
            } else if (c == '\r') {
                // CR only: increment if NOT followed by LF
                if (i + 1 >= text.length() || text.charAt(i + 1) != '\n') {
                    lineNum++;
                }
            }
        }
        return lineNum;
    }
}
