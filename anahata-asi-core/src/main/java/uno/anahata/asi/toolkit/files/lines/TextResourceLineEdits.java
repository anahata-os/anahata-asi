/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files.lines;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import uno.anahata.asi.toolkit.files.AbstractTextResourceWrite;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

/**
 * The next-generation surgical line editor for AGI.
 * <p>
 * This DTO replaces arithmetic requirements with semantic intent. It separates 
 * insertions, replacements, and deletions into distinct lists to maximize 
 * model precision.
 * </p>
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Schema(description = "A set of semantic line edits (insertions, replacements, deletions) targeting 1-based line numbers on a resource in the RAG message.")
public class TextResourceLineEdits extends AbstractTextResourceWrite {

    @Schema(description = "List of insertions (adding code without removal).")
    private List<LineInsertion> insertions = new ArrayList<>();

    @Schema(description = "List of range replacements.")
    private List<LineReplacement> replacements = new ArrayList<>();

    @Schema(description = "List of range deletions.")
    private List<LineDeletion> deletions = new ArrayList<>();

    public TextResourceLineEdits(String uuid, long lastModified) {
        super(uuid, lastModified);
    }

    @Override
    public String calculateResultingContent(String currentContent) throws Exception {
        String separator = currentContent.contains("\r\n") ? "\r\n" : "\n";
        List<String> lines = new ArrayList<>(Arrays.asList(currentContent.split("\\R", -1)));

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

        // Sort DESCENDING by line number to maintain coordinate stability during application
        allEdits.sort(Comparator.comparingInt(AbstractLineEdit::getSortLine).reversed());

        for (AbstractLineEdit edit : allEdits) {
            edit.apply(lines);
        }

        return String.join(separator, lines);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Performs pre-flight normalization of coordinates. 
     * Specifically, it clips 'Integer.MAX_VALUE' markers to the actual line count 
     * of the resource to prevent visualizer crashes in the IDE.
     * </p>
     */
    @Override
    public void validate(uno.anahata.asi.agi.Agi agi) throws Exception {
        super.validate(agi);

        uno.anahata.asi.agi.resource.Resource res = agi.getResourceManager().get(resourceUuid);
        // Calculate line count once for normalization
        int lineCount = res.asText().split("\\R", -1).length;

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

        // 2. Perform Overlap Detection
        List<AbstractLineEdit> sorted = new ArrayList<>();
        if (insertions != null) sorted.addAll(insertions);
        if (replacements != null) sorted.addAll(replacements);
        if (deletions != null) sorted.addAll(deletions);
        
        sorted.sort(Comparator.comparingInt(AbstractLineEdit::getSortLine));
        
        int lastEnd = -1;
        for (AbstractLineEdit edit : sorted) {
            int start = edit.getSortLine();
            if (start <= lastEnd) {
                throw new uno.anahata.asi.agi.tool.AiToolException("Overlapping surgical edits detected at line " + start);
            }
            
            if (edit instanceof LineReplacement rep) {
                lastEnd = rep.getEndLine();
            } else if (edit instanceof LineDeletion del) {
                lastEnd = del.getEndLine();
            } else {
                // Insertions are point-targets, they don't advance the 'lastEnd' for replacement checks
                // but we prevent multiple insertions at the exact same point to maintain intent clarity.
                lastEnd = start; 
            }
        }
    }
}
