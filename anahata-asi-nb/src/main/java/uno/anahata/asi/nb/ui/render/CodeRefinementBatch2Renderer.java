/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.render;

import java.util.ArrayList;
import java.util.List;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.nb.tools.java.coderefiner.CodeRefinementBatch;
import uno.anahata.asi.nb.tools.java.coderefiner.CodeRefinementIntent;
import uno.anahata.asi.toolkit.resources.text.LineComment;

/**
 * Specialized renderer for the robust {@link CodeRefinementBatch}.
 * <p>
 * This renderer provides a surgical dashboard at the top of the diff viewer 
 * that lists all structural intents in their flattened format, ensuring 
 * clear feedback for the user before committing the AST changes.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class CodeRefinementBatch2Renderer extends AbstractTextResourceWriteRenderer<CodeRefinementBatch> {

    /** {@inheritDoc} */
    @Override
    protected List<LineComment> getLineComments() {
        // We rely on the Intent Panel for semantic context of structural changes.
        // mapping AST trees back to static line numbers for bubbles is non-trivial 
        // without replaying the full surgery.
         return update.getCalculatedComments();
    }

    /** {@inheritDoc} */
    @Override
    protected JComponent createIntentPanel() {
        JPanel panel = new JPanel(new MigLayout("fillx, insets 0", "[grow]", "[]"));
        panel.setOpaque(false);
        
        JLabel title = new JLabel("<html><b>Surgical AST Intents (V2-Flattened):</b></html>");
        panel.add(title, "wrap");
        
        if (update.getIntents() != null) {
            for (CodeRefinementIntent intent : update.getIntents()) {
                JLabel label = new JLabel("<html>" + intent.getHtmlDisplay() + "</html>");
                label.setToolTipText("Structural Modification: " + intent.getType());
                panel.add(label, "gapleft 15, wrap");
            }
        }
        
        return panel;
    }

    /** {@inheritDoc} */
    @Override
    protected CodeRefinementBatch createUpdatedDto(String newContent) {
        CodeRefinementBatch batch = new CodeRefinementBatch();
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
