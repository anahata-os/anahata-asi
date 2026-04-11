/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.resources.text.lines;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import uno.anahata.asi.toolkit.resources.text.AbstractTextResourceWrite;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import uno.anahata.asi.agi.tool.AgiToolException;

/**
 * The next-generation surgical line editor for AGI.
 * <p>
 * This DTO replaces traditional arithmetic patching with a semantic model
 * that separates insertions, replacements, and deletions into discrete
 * intent-based lists. This ensures maximum coordinate stability and
 * minimizes the risk of conflicting updates.
 * </p>
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Schema(description = "A set of semantic line edits (insertions, replacements, deletions) targeting 1-based line numbers on a resource in the RAG message.")
public class TextResourceLineEdits extends AbstractTextResourceWrite {

    /**
     * The list of atomic point-insertions to be applied.
     * <p>
     * Insertions are logically applied "above" the target coordinate, ensuring
     * that the original line content is pushed downwards.
     * </p>
     */
    @Schema(description = "List of insertions (adding code without removal).")
    private List<LineInsertion> insertions = new ArrayList<>();

    /**
     * The list of range-based replacements to be applied.
     * <p>
     * Strictly for replacing existing content; should not be used for pure
     * insertions where no lines are removed.
     * </p>
     */
    @Schema(description = "List of range replacements. Do not use for pure insertions. Strictly for the lines to be replaced. Do not include surrounding context (existing lines).")
    private List<LineReplacement> replacements = new ArrayList<>();

    /**
     * The list of range-based deletions to be applied.
     * <p>
     * Requires explicit coordinate and checksum validation during the
     * application phase.
     * </p>
     */
    @Schema(description = "List of range deletions.")
    private List<LineDeletion> deletions = new ArrayList<>();

    public TextResourceLineEdits(String uuid, long lastModified) {
        super(uuid, lastModified);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Aggregates all edits, performs a stable
     * descending sort, and applies mutations to the line list to generate
     *  the final resulting content.
     * </p>
     */
    @Override
    public String calculateResultingContent() throws Exception {
        if (originalContent == null) {
            throw new AgiToolException("Logic Error: calculateResultingContent called before captureOriginalContent");
        }
        String separator = originalContent.contains("\r\n") ? "\r\n" : "\n";
        List<String> lines = new ArrayList<>(Arrays.asList(originalContent.split("\\R", -1)));

        // Aggregate all edits
        List<AbstractLineEdit> allEdits = new ArrayList<>();
        if (insertions != null) {
            allEdits.addAll(insertions);
        }
        if (replacements != null) {
            allEdits.addAll(replacements);
        }
        if (deletions != null) {
            allEdits.addAll(deletions);
        }

        // Sort DESCENDING by line number with tie-breaker for application order.
        // For the same coordinate, the range edit (replacement/deletion) must be applied 
        // before the point insertion to ensure logical space is cleared before push-down.
        allEdits.sort(new SurgicalEditComparator().reversed());

        for (AbstractLineEdit edit : allEdits) {
            edit.apply(lines);
        }

        return String.join(separator, lines);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Performs critical overlap detection across all
     * discrete edit lists. Ensures that no two operations target the same
     * coordinate range, protecting file integrity before mutation.
     * </p>
     */
    @Override
    public void validate(uno.anahata.asi.agi.Agi agi) throws Exception {
        super.validate(agi);

        /*
        // Calculate line count using the authoritative snapshot
        int lineCount = originalContent.split("\\R", -1).length;

        // 1. Normalize 'Magic' coordinates
        if (replacements != null) {
            for (LineReplacement rep : replacements) {
                if (rep.getEndLine() == Integer.MAX_VALUE) {
                    rep.setEndLine(lineCount);
                }
            }
        }

        if (deletions != null) {
            for (LineDeletion del : deletions) {
                if (del.getEndLine() == Integer.MAX_VALUE) {
                    del.setEndLine(lineCount);
                }
            }
        }
         */
        
        
        // 2. Perform Overlap Detection
        List<AbstractLineEdit> sorted = new ArrayList<>();
        if (insertions != null) {
            sorted.addAll(insertions);
        }
        if (replacements != null) {
            sorted.addAll(replacements);
        }
        if (deletions != null) {
            sorted.addAll(deletions);
        }

        // Ascending sort with tie-breaker: point-insertions come before range-edits for the same coordinate.
        sorted.sort(new SurgicalEditComparator());

        int lastEnd = -1;
        int lastInsertionPoint = -1;
        for (AbstractLineEdit edit : sorted) {
            int start = edit.getSortLine();

            if (edit instanceof LineInsertion) {
                if (start <= lastEnd) {
                    throw new uno.anahata.asi.agi.tool.AgiToolException("Overlapping surgical edits: insertion at line " + start + " is inside a previous range ending at " + lastEnd);
                }
                if (start == lastInsertionPoint) {
                    throw new uno.anahata.asi.agi.tool.AgiToolException("Overlapping surgical edits: multiple insertions at the exact same line " + start);
                }
                lastInsertionPoint = start;
            } else {
                // For range edits (replacement/deletion)
                if (start <= lastEnd) {
                    throw new uno.anahata.asi.agi.tool.AgiToolException("Overlapping surgical edits: range starting at line " + start + " overlaps with a previous range ending at " + lastEnd);
                }
                // Note: start == lastInsertionPoint is ALLOWED. The insertion is logically "above" the range.
                lastEnd = (edit instanceof LineReplacement rep) ? rep.getEndLine() : ((LineDeletion) edit).getEndLine();
            }
        }
    }
}
