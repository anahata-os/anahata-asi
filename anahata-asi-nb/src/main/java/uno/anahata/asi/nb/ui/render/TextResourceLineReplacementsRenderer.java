/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.render;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import uno.anahata.asi.toolkit.files.LineReplacement;
import uno.anahata.asi.toolkit.files.LineComment;
import uno.anahata.asi.toolkit.files.TextResourceLineReplacements;

/**
 * A rich renderer for {@link TextResourceLineReplacements} tool parameters.
 * It provides a preview of line-based replacements in the NetBeans diff viewer.
 * 
 * <p>This renderer uses a chronological mapping strategy to project original 
 * line numbers onto their final positions in the proposed content panel, 
 * ensuring that AI comments stay aligned with the changes.</p>
 * 
 * @author anahata
 */
public class TextResourceLineReplacementsRenderer extends AbstractTextResourceWriteRenderer<TextResourceLineReplacements> {

    /** {@inheritDoc} */
    @Override
    protected String calculateProposedContent(String currentContent) throws Exception {
        return update.performReplacements(currentContent);
    }

    /** {@inheritDoc} */
    @Override
    protected List<LineComment> getLineComments(String currentContent) {
        List<LineComment> comments = new ArrayList<>();
        if (update.getReplacements() == null) {
            return comments;
        }

        // Sort ascending to calculate cumulative shift for the proposed (right) editor
        List<LineReplacement> sorted = new ArrayList<>(update.getReplacements());
        sorted.sort(Comparator.comparingInt(LineReplacement::getStartLine));

        int cumulativeShift = 0;
        for (LineReplacement lr : sorted) {
            if (lr.getReason() != null && !lr.getReason().isBlank()) {
                // Map the original line number to the proposed line number in the right panel
                int proposedLine = lr.getStartLine() + cumulativeShift;
                comments.add(new LineComment(proposedLine, lr.getReason()));
            }

            // Calculate shift: lines added minus lines removed using shared utility
            int added = DiffCommentUtils.getLineCount(lr.getReplacement());
            int removed = lr.getLineCount();
            cumulativeShift += (added - removed);
        }
        return comments;
    }

    /** {@inheritDoc} */
    @Override
    protected TextResourceLineReplacements createUpdatedDto(String newContent) {
        // Full override logic for user manual edits
        LineReplacement fullOverride = LineReplacement.builder()
                .startLine(1)
                .lineCount(Integer.MAX_VALUE) 
                .replacement(newContent)
                .reason("User manual edit")
                .build();
        
        TextResourceLineReplacements dto = new TextResourceLineReplacements(
                update.getResourceUuid(),
                update.getLastModified(),
                List.of(fullOverride)
        );
        dto.setOriginalContent(update.getOriginalContent());
        return dto;
    }

    /** {@inheritDoc} */
    @Override
    protected int getInitialTabIndex() {
        return 0; // Line-based changes are best viewed in Graphical diff
    }
}
