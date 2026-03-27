/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.render;
import java.awt.Component;
import java.awt.Color;
import java.awt.Font;
import javax.swing.Box;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import uno.anahata.asi.toolkit.resources.text.LineComment;
import uno.anahata.asi.toolkit.resources.text.lines.AbstractLineEdit;
import uno.anahata.asi.toolkit.resources.text.lines.LineDeletion;
import uno.anahata.asi.toolkit.resources.text.lines.LineInsertion;
import uno.anahata.asi.toolkit.resources.text.lines.LineReplacement;
import uno.anahata.asi.toolkit.resources.text.lines.TextResourceLineEdits;

/**
 * A rich renderer for the next-generation {@link TextResourceLineEdits} tool parameters.
 * Provides a high-fidelity diff preview with cumulative coordinate shifting for AI comments.
 * 
 * @author anahata
 */
public class TextResourceLineEditsRenderer extends AbstractTextResourceWriteRenderer<TextResourceLineEdits> {

    /** {@inheritDoc} */
    @Override
    protected JComponent createIntentPanel() {
        JPanel panel = new JPanel();
        panel.setLayout(new javax.swing.BoxLayout(panel, javax.swing.BoxLayout.Y_AXIS));
        panel.setAlignmentX(Component.LEFT_ALIGNMENT);
        panel.setOpaque(false);

        addEditsToPanel(panel, "Insertions", update.getInsertions(), 
                ins -> "Before Line " + ins.getAtLine() + " (" + DiffCommentUtils.getLineCount(ins.getContent()) + " lines): " + ins.getReason());
        
        addEditsToPanel(panel, "Replacements", update.getReplacements(), 
                rep -> "Lines " + rep.getStartLine() + "-" + rep.getEndLine() + " (" + DiffCommentUtils.getLineCount(rep.getContent()) + " lines): " + rep.getReason());
        addEditsToPanel(panel, "Deletions", update.getDeletions(), 
                del -> "Lines " + del.getStartLine() + "-" + del.getEndLine() + " (" + del.getExpectedCount() + " lines): " + del.getReason());

        return panel.getComponentCount() > 0 ? panel : null;
    }

    /**
     * Helper to add a group of edits to the intent panel with a semantic header.
     */
    private <E> void addEditsToPanel(JPanel panel, String title, List<E> edits, java.util.function.Function<E, String> formatter) {
        if (edits == null || edits.isEmpty()) {
            return;
        }
        
        JLabel titleLabel = new JLabel(title + ":");
        titleLabel.setFont(titleLabel.getFont().deriveFont(Font.BOLD, 11));
        titleLabel.setForeground(Color.DARK_GRAY);
        titleLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        panel.add(titleLabel);
        
        for (E edit : edits) {
            JLabel editLabel = new JLabel("  • " + formatter.apply(edit));
            editLabel.setFont(new Font("SansSerif", Font.PLAIN, 11));
            editLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
            panel.add(editLabel);
        }
        panel.add(Box.createVerticalStrut(5));
    }

    /** {@inheritDoc} */
    @Override
    protected List<LineComment> getLineComments() {
        List<LineComment> comments = new ArrayList<>();
        String content = update.getOriginalContent();
        if (content == null) {
            return comments;
        }
        
        // 1. Aggregate and sort edits by their original start line
        List<AbstractLineEdit> allEdits = new ArrayList<>();
        if (update.getInsertions() != null) allEdits.addAll(update.getInsertions());
        if (update.getReplacements() != null) allEdits.addAll(update.getReplacements());
        if (update.getDeletions() != null) allEdits.addAll(update.getDeletions());
        
        allEdits.sort(Comparator.comparingInt(AbstractLineEdit::getSortLine));

        int cumulativeShift = 0;
        for (AbstractLineEdit edit : allEdits) {
            if (edit.getReason() != null && !edit.getReason().isBlank()) {
                // The comment should point to the line in the "Proposed" view
                int proposedLine = edit.getSortLine() + cumulativeShift;
                comments.add(new LineComment(proposedLine, edit.getReason()));
            }

            // Update cumulative shift based on edit type
            if (edit instanceof LineInsertion ins) {
                cumulativeShift += DiffCommentUtils.getLineCount(ins.getContent());
            } else if (edit instanceof LineReplacement rep) {
                int added = DiffCommentUtils.getLineCount(rep.getContent());
                int removed = (rep.getEndLine() - rep.getStartLine()) + 1;
                cumulativeShift += (added - removed);
            } else if (edit instanceof LineDeletion del) {
                int removed = (del.getEndLine() - del.getStartLine()) + 1;
                cumulativeShift -= removed;
            }
        }
        return comments;
    }

    /** {@inheritDoc} */
    @Override
    protected TextResourceLineEdits createUpdatedDto(String newContent) {
        // When the user manually edits the diff, we treat it as a full file replacement
        LineReplacement fullOverride = new LineReplacement();
        fullOverride.setStartLine(1);
        fullOverride.setEndLine(DiffCommentUtils.getLineCount(update.getOriginalContent())); 
        fullOverride.setContent(newContent);
        fullOverride.setReason("User manual edit");
        
        TextResourceLineEdits dto = new TextResourceLineEdits(
                update.getResourceUuid(),
                update.getLastModified()
        );
        dto.getReplacements().add(fullOverride);
        dto.setOriginalContent(update.getOriginalContent());
        dto.setOriginalResourceName(update.getOriginalResourceName());
        return dto;
    }

    /** {@inheritDoc} */
    @Override
    protected int getInitialTabIndex() {
        // Now using recommendedTabIndex from base class validation, but we can override if needed
        return super.getInitialTabIndex();
    }
}
