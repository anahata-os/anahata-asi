/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.resources.text;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.tool.AgiToolException;

/**
 * Represents a set of text replacement operations for a specific file. Extends
 * AbstractTextFileWrite to inherit path and optimistic locking.
 *
 * @author anahata
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Getter
@EqualsAndHashCode(callSuper = true)
@Schema(description = "Represents a set of text replacement operations for a specific resource.")
public class TextResourceReplacements extends AbstractTextResourceWrite {

    /**
     * The list of replacements to perform in this file.
     */
    @Schema(description = "The list of replacements to perform in this file.", required = true)
    private List<TextReplacement> replacements;

    @Builder
    public TextResourceReplacements(String resourceUuid, long lastModified, List<TextReplacement> replacements) {
        super(resourceUuid, lastModified);
        this.replacements = replacements;
    }

    /** {@inheritDoc} */
    @Override
    public String calculateResultingContent() throws Exception {
        if (originalContent == null) {
            throw new AgiToolException("Logic Error: calculateResultingContent called before captureOriginalContent");
        }
        String newContent = originalContent;
        for (TextReplacement replacement : replacements) {
            String target = replacement.getTarget();
            if (target == null || target.isEmpty()) {
                continue;
            }

            // Create a regex that is lenient with whitespace and line endings
            String regex = Stream.of(target.split("\\R", -1))
                    .map(line -> "[ \\t]*" + Pattern.quote(line.trim()) + "[ \\t]*")
                    .collect(Collectors.joining("\\R"));

            newContent = newContent.replaceAll(regex, Matcher.quoteReplacement(replacement.getReplacement()));
        }
        return newContent;
    }

    /** {@inheritDoc} */
    @Override
    public void validate(Agi agi) throws Exception {
        // Capture original content before subclass validation
        captureOriginalContent(agi);

        if (replacements == null || replacements.isEmpty()) {
             throw new AgiToolException("No replacements provided.");
        }

        String normalizedOriginal = normalizeForComparison(originalContent);

        for (TextReplacement replacement : replacements) {
            String target = replacement.getTarget();
            if (target == null || target.isEmpty()) {
                throw new AgiToolException("Replacement target cannot be null or empty.");
            }
            
            String normalizedTarget = normalizeForComparison(target);
            int count = StringUtils.countMatches(normalizedOriginal, normalizedTarget);
            
            int expected = replacement.getExpectedCount();
            
            if (expected >= 0 && count != expected) {
                throw new AgiToolException("Strict count mismatch for target [" + target.substring(0, Math.min(20, target.length())) + "...]. Expected " + expected + ", but found " + count);
            }
            
            if (expected == -1 && count == 0) {
                 throw new AgiToolException("Mandatory target string not found (even after normalization): " + target);
            }
            
            // If expected == -2, we proceed even if count is 0.
        }
        
        // Finally call super.validate to check lastModified and perform identical check
        super.validate(agi);
    }

    /**
     * Normalizes a string for "semantic" comparison by standardizing line 
     * endings and removing leading and trailing whitespace from all lines.
     * 
     * @param s The string to normalize.
     * @return The normalized string.
     */
    private String normalizeForComparison(String s) {
        if (s == null) {
            return null;
        }
        // 1. Standardize line endings to LF
        String result = s.replace("\r\n", "\n").replace("\r", "\n");
        // 2. Remove leading and trailing whitespace from each line
        return Stream.of(result.split("\\R", -1))
                .map(String::trim)
                .collect(Collectors.joining("\n"));
    }
}
