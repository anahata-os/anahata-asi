/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.resources.text;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.Objects;
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

    /**
     * Constructs a new TextResourceReplacements using the builder pattern.
     *
     * @param resourceUuid The unique identifier of the target text resource.
     * @param lastModified Optimistic locking: the expected last modified timestamp of the file.
     * @param replacements The list of text replacements to perform.
     */
    @Builder
    public TextResourceReplacements(String resourceUuid, long lastModified, List<TextReplacement> replacements) {
        super(resourceUuid, lastModified);
        this.replacements = replacements;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String doCalculateResultingContent(Agi agi) throws Exception {
        if (originalContent == null) {
            throw new AgiToolException("Logic Error: calculateResultingContent called before captureOriginalContent");
        }
        String newContent = originalContent;
        // Process replacements in reverse order if we were doing index-based, but since we work on the whole string 
        // per replacement, we'll just iterate. Note: overlapping replacements are not supported.
        for (TextReplacement replacement : replacements) {
            String target = replacement.getTarget();
            if (target == null || target.isEmpty()) {
                continue;
            }

            // Normalize target to LF for regex building
            String targetLF = target.replace("\r\n", "\n").replace("\r", "\n");

            // Create a regex that matches the exact text but is lenient ONLY with line endings (\r\n vs \n)
            String regex = Stream.of(targetLF.split("\\n", -1))
                    .map(Pattern::quote)
                    .collect(Collectors.joining("\\r?\\n"));

            Pattern pattern = Pattern.compile(regex);
            Matcher matcher = pattern.matcher(newContent);

            List<Integer> indexes = replacement.getOccurrenceIndexes();
            if (indexes != null && !indexes.isEmpty()) {
                StringBuilder sb = new StringBuilder();
                int count = 0;
                while (matcher.find()) {
                    count++;
                    if (indexes.contains(count)) {
                        matcher.appendReplacement(sb, Matcher.quoteReplacement(replacement.getReplacement()));
                    }
                }
                matcher.appendTail(sb);
                newContent = sb.toString();
            } else {
                newContent = matcher.replaceAll(Matcher.quoteReplacement(replacement.getReplacement()));
            }
        }
        return newContent;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void validate(Agi agi) throws Exception {
        // 1. Authoritative state capture and locking check
        validateStructuralState(agi);

        if (replacements == null || replacements.isEmpty()) {
            throw new AgiToolException("No replacements provided.");
        }

        String normalizedOriginal = normalizeForComparison(originalContent);
        String lenientOriginal = normalizeWhitespaces(originalContent);

        for (TextReplacement replacement : replacements) {
            int replacementIndex = replacements.indexOf(replacement);
            String target = replacement.getTarget();
            if (target == null || target.isEmpty()) {
                throw new AgiToolException("Replacement target cannot be null or empty.");
            }

            String normalizedTarget = normalizeForComparison(target);
            String lenientTarget = normalizeWhitespaces(target);
            int count = StringUtils.countMatches(normalizedOriginal, normalizedTarget);
            int lenientCount = StringUtils.countMatches(lenientOriginal, lenientTarget);

            int expected = replacement.getTotalOccurrences();
            List<Integer> indexes = replacement.getOccurrenceIndexes();
            boolean bothIdentical = Objects.equals(target, replacement.getReplacement());
            boolean normalizedIdentical = Objects.equals(normalizedTarget, replacement.getReplacement());
            boolean lenientPasses = lenientCount == expected;
            String diagnostics = "Surgical Checksum Failed for replacement #" + replacementIndex
                        + "\n-target            : [" + StringUtils.abbreviateMiddle(target, "...", 256) + "]. "
                        + "\n-normalized target : [" + StringUtils.abbreviateMiddle(normalizedTarget, "...", 256) + "]. "
                        + "\n-replacement       : [" + StringUtils.abbreviateMiddle(replacement.getReplacement(), "...", 256) + "]. "
                        + "\n-target and replacement identical: " + bothIdentical
                        + "\n-normalized target and replacement identical: " + normalizedIdentical
                        + "\n-would have passed validation with normalized whitespaces: " + lenientPasses
                        + "\n-reason :" + replacement.getReason()
                        + "\n-totalOccurrences:" + replacement.getTotalOccurrences()
                        + "\n-occurrenceIndexes:" + replacement.getOccurrenceIndexes();
            
            if (count != expected) {
                throw new AgiToolException(diagnostics 
                        + "\n**Your 'totalOccurrences' was " + expected + " but I found " + count + " matches in the file** "
                        + "\nYou have to provide the exact number of 'totalOccurences' in the file for each replacement.");

            }

            if (indexes != null) {
                for (Integer idx : indexes) {
                    if (idx > count || idx <= 0) {
                        throw new AgiToolException(diagnostics
                                + "\nSurgical Range Error: **Requested occurrence index " + idx + " but only " + count + " occurrences found.**");
                    }
                }
            }
        }

        // 3. Final check for identical content
        validateIdenticalContent(agi);
    }

    /**
     * Normalizes a string for comparison by standardizing line endings to LF.
     * Whitespace is preserved exactly to prevent silent destruction of
     * indentation.
     *
     * @param s The string to normalize.
     * @return The normalized string.
     */
    private String normalizeForComparison(String s) {
        if (s == null) {
            return null;
        }
        // Standardize line endings to LF
        return s.replace("\r\n", "\n").replace("\r", "\n");
    }

    /**
     * Normalizes a string by standardizing line endings and removing trailing
     * whitespace from each line for lenient diagnostic comparison.
     *
     * @param s The string to normalize.
     * @return The normalized string.
     */
    private String normalizeWhitespaces(String s) {
        if (s == null) {
            return null;
        }
        return Stream.of(s.replace("\r\n", "\n").replace("\r", "\n").split("\\n", -1))
                .map(str -> StringUtils.stripEnd(str, null))
                .collect(Collectors.joining("\n"));
    }
}
