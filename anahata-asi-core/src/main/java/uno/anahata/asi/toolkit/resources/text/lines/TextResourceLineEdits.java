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
import uno.anahata.asi.agi.tool.AiToolException;

/**
 * The next-generation surgical line editor for AGI.
 * <p>
 * This DTO replaces arithmetic requirements with semantic intent. It separates
 * insertions, replacements, and deletions into distinct lists to maximize model
 * precision.
 * </p>
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Schema(description = "A set of semantic line edits (insertions, replacements, deletions) targeting 1-based line numbers on a resource in the RAG message.")
public class TextResourceLineEdits extends AbstractTextResourceWrite {

    @Schema(description = "List of insertions (adding code without removal).")
    private List<LineInsertion> insertions = new ArrayList<>();

    @Schema(description = "List of range replacements. Do not use for pure insertions. Strictly for the lines to be replaced. Do not include surrounding context (existing lines).")
    private List<LineReplacement> replacements = new ArrayList<>();

    @Schema(description = "List of range deletions.")
    private List<LineDeletion> deletions = new ArrayList<>();

    public TextResourceLineEdits(String uuid, long lastModified) {
        super(uuid, lastModified);
    }

    @Override
    public String calculateResultingContent() throws Exception {
        if (originalContent == null) {
            throw new AiToolException("Logic Error: calculateResultingContent called before captureOriginalContent");
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
     * Implementation details: Performs pre-flight normalization of coordinates.
     * Specifically, it clips 'Integer.MAX_VALUE' markers to the actual line
     * count of the resource to prevent visualizer crashes in the IDE.
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
                    throw new uno.anahata.asi.agi.tool.AiToolException("Overlapping surgical edits: insertion at line " + start + " is inside a previous range ending at " + lastEnd);
                }
                if (start == lastInsertionPoint) {
                    throw new uno.anahata.asi.agi.tool.AiToolException("Overlapping surgical edits: multiple insertions at the exact same line " + start);
                }
                lastInsertionPoint = start;
            } else {
                // For range edits (replacement/deletion)
                if (start <= lastEnd) {
                    throw new uno.anahata.asi.agi.tool.AiToolException("Overlapping surgical edits: range starting at line " + start + " overlaps with a previous range ending at " + lastEnd);
                }
                // Note: start == lastInsertionPoint is ALLOWED. The insertion is logically "above" the range.
                lastEnd = (edit instanceof LineReplacement rep) ? rep.getEndLine() : ((LineDeletion) edit).getEndLine();
            }
        }
    }
}
