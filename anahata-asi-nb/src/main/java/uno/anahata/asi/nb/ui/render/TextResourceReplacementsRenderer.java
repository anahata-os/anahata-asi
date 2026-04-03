/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.render;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import uno.anahata.asi.toolkit.resources.text.LineComment;
import uno.anahata.asi.toolkit.resources.text.TextReplacement;
import uno.anahata.asi.toolkit.resources.text.TextResourceReplacements;

/**
 * A rich renderer for {@link TextResourceReplacements} tool parameters.
 * It provides a preview of surgical replacements in the NetBeans diff viewer
 * and automatically maps replacement reasons to line-level gutter comments.
 * 
 * <p>This renderer uses a chronological mapping strategy to ensure that comments 
 * stay aligned with the proposed content even when multiple replacements change 
 * the file's line count.</p>
 * 
 * @author anahata
 */
public class TextResourceReplacementsRenderer extends AbstractTextResourceWriteRenderer<TextResourceReplacements> {

    /** {@inheritDoc} */
    @Override
    protected List<LineComment> getLineComments() {
        List<LineComment> comments = new ArrayList<>();
        String content = update.getOriginalContent();
        if (content == null || update.getReplacements() == null || update.getReplacements().isEmpty()) {
            return comments;
        }

        // 1. Identify all occurrences of all targets in the original content
        record ReplacementEvent(TextReplacement tr, int index) {}
        List<ReplacementEvent> events = new ArrayList<>();
        
        for (TextReplacement tr : update.getReplacements()) {
            String target = tr.getTarget();
            if (target == null || target.isEmpty()) {
                continue;
            }
            
            int idx = content.indexOf(target);
            while (idx != -1) {
                events.add(new ReplacementEvent(tr, idx));
                idx = content.indexOf(target, idx + target.length());
                // Avoid infinite loops if replacement contains target
                if (tr.getReplacement() != null && tr.getReplacement().contains(target)) {
                    break;
                }
            }
        }

        // 2. Sort events by character position to ensure accurate cumulative line-shifting
        events.sort(Comparator.comparingInt(ReplacementEvent::index));

        // 3. Map to proposed line numbers using cumulative shift
        int cumulativeLineShift = 0;
        for (ReplacementEvent event : events) {
            TextReplacement tr = event.tr();
            int originalLine = DiffCommentUtils.getLineAt(content, event.index());
            int proposedLine = originalLine + cumulativeLineShift;
            
            if (tr.getReason() != null && !tr.getReason().isBlank()) {
                comments.add(new LineComment(proposedLine, tr.getReason()));
            }
            
            // Calculate shift: lines added minus lines removed
            int removed = DiffCommentUtils.getLineCount(tr.getTarget());
            int added = DiffCommentUtils.getLineCount(tr.getReplacement());
            cumulativeLineShift += (added - removed);
        }
        
        return comments;
    }

    /** {@inheritDoc} 
     * <p>Creates a new DTO representing the user's manual edits. This effectively 
     * treats the manual edit as a full-content replacement of the original 
     * state to ensure validation passes.</p>
     */
    @Override
    protected TextResourceReplacements createUpdatedDto(String newContent) {
        TextResourceReplacements dto = new TextResourceReplacements(
                update.getResourceUuid(),
                update.getLastModified(),
                List.of(TextReplacement.builder()
                        .target(update.getOriginalContent()) 
                        .replacement(newContent)
                        .reason("User manual edit")
                        .expectedCount(1)
                        .build())
        );
        dto.setOriginalContent(update.getOriginalContent());
        dto.setOriginalResourceName(update.getOriginalResourceName());
        return dto;
    }

    /** {@inheritDoc} */
    @Override
    protected int getInitialTabIndex() {
        return 1; // Default to Textual tab for search-and-replace
    }
}
