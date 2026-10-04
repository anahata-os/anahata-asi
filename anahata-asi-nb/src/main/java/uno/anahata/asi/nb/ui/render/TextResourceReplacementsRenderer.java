/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.render;

import uno.anahata.asi.toolkit.resources.text.DiffCommentUtils;
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
        return update.calculateLineComments(agiPanel.getAgi());
    }

    /** {@inheritDoc} 
     * <p>Creates a new DTO representing the user's manual edits. This effectively 
     * treats the manual edit as a full-content replacement of the original 
     * state to ensure validation passes.</p>
     */
    @Override
    protected TextResourceReplacements createUpdatedDto(String newContent) {
        TextResourceReplacements dto = new TextResourceReplacements();
        dto.setResourceUuid(update.getResourceUuid());
        dto.setLastModified(update.getLastModified());
        dto.setManualOverride(newContent);
        // Preserve intents so the UI still shows what we *tried* to do even if we overrode it
        dto.setReplacements(update.getReplacements());
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
