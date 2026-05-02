/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.render;

import java.util.ArrayList;
import java.util.List;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.nb.tools.java.coderefiner.CodeRefinementBatchPolymorphic;
import uno.anahata.asi.nb.tools.java.coderefiner.CodeRefinementIntentPolymorphic;
import uno.anahata.asi.nb.tools.java.coderefiner.DeleteMemberIntent;
import uno.anahata.asi.nb.tools.java.coderefiner.InsertMemberIntent;
import uno.anahata.asi.nb.tools.java.coderefiner.MoveMemberIntent;
import uno.anahata.asi.nb.tools.java.coderefiner.UpdateMemberIntent;
import uno.anahata.asi.toolkit.resources.text.LineComment;

/**
 * Specialized renderer for Java refinement batches.
 * <p>
 * Provides a surgical dashboard at the top of the diff viewer that lists 
 * all structural intents (Insert, Update, Delete, Move) before showing 
 * the unified code diff.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class CodeRefinementBatchRenderer extends AbstractTextResourceWriteRenderer<CodeRefinementBatchPolymorphic> {

    @Override
    protected List<LineComment> getLineComments() {
        // Structural AST changes don't produce static line comments in the same way 
        // full text replacements do. We rely on the Intent Panel for semantic context.
        return new ArrayList<>();
    }

    @Override
    protected JComponent createIntentPanel() {
        JPanel panel = new JPanel(new MigLayout("fillx, insets 0", "[grow]", "[]"));
        panel.setOpaque(false);
        
        JLabel title = new JLabel("<html><b>Surgical AST Intents:</b></html>");
        panel.add(title, "wrap");
        
        if (update.getIntents() != null) {
            for (CodeRefinementIntentPolymorphic intent : update.getIntents()) {
                log.info("Creating intent panel for " + intent);
                JLabel label = new JLabel("<html>" + intent.getHtmlDisplay() + "</html>");
                label.setToolTipText(intent.toString());
                panel.add(label, "gapleft 15, wrap");
            }
        }
        
        return panel;
    }

    @Override
    protected CodeRefinementBatchPolymorphic createUpdatedDto(String newContent) {
        CodeRefinementBatchPolymorphic batch = new CodeRefinementBatchPolymorphic();
        batch.setResourceUuid(update.getResourceUuid());
        batch.setLastModified(update.getLastModified());
        batch.setManualOverride(newContent);
        // Preserve intents so the UI still shows what we *tried* to do even if we overrode it
        batch.setIntents(update.getIntents());
        batch.setOptimize(update.isOptimize());
        batch.setSave(update.isSave());
        batch.setOriginalContent(update.getOriginalContent());
        batch.setOriginalResourceName(update.getOriginalResourceName());
        return batch;
    }
}
