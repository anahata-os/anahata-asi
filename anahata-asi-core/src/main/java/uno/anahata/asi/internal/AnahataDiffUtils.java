/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.internal;

import com.github.difflib.UnifiedDiffUtils;
import com.github.difflib.patch.Patch;
import java.util.Arrays;
import java.util.List;

/**
 * Utility for generating standard unified diffs using java-diff-utils.
 * <p>
 * This provides the model with industry-standard context of text modifications, 
 * making it much easier for the ASI to reason about the delta of its own actions.
 * </p>
 * 
 * @author anahata
 */
public class AnahataDiffUtils {

    /**
     * Generates a unified diff between two strings.
     * 
     * @param originalName The name/path of the original file (for the diff header).
     * @param original The base content.
     * @param revised The new content.
     * @return A standard unified diff string, or an empty string if there are no differences.
     */
    public static String generateUnifiedDiff(String originalName, String original, String revised) {
        if (original.equals(revised)) {
            return "";
        }

        List<String> originalLines = Arrays.asList(original.split("\\R", -1));
        List<String> revisedLines = Arrays.asList(revised.split("\\R", -1));

        Patch<String> patch = com.github.difflib.DiffUtils.diff(originalLines, revisedLines);
        List<String> unifiedDiff = UnifiedDiffUtils.generateUnifiedDiff(originalName, originalName, originalLines, patch, 3);

        return String.join("\n", unifiedDiff);
    }
}
