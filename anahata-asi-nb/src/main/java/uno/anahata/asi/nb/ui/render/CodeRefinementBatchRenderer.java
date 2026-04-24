/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.render;

import java.util.ArrayList;
import java.util.List;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.nb.tools.java.coderefiner.CodeRefinementBatch;
import uno.anahata.asi.nb.tools.java.coderefiner.CodeRefinementIntent;
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
public class CodeRefinementBatchRenderer extends AbstractTextResourceWriteRenderer<CodeRefinementBatch> {

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
        
        for (CodeRefinementIntent intent : update.getIntents()) {
            String desc = "Structural Modification";
            String icon = "🛠️";
            
            if (intent instanceof InsertMemberIntent ins) {
                String decl = ins.getDeclaration();
                String memberType = (decl != null && decl.contains("(")) ? "Method" : "Field";
                desc = "<b>Insert " + memberType + "</b> " + ins.getPosition() + " " + ins.getAnchorMemberName();
                icon = "<font color='#4CAF50'>[+]</font>";
            } else if (intent instanceof UpdateMemberIntent upd) {
                desc = "<b>Update</b> " + getSimpleName(upd.getMemberFqn());
                icon = "<font color='#2196F3'>[*]</font>";
            } else if (intent instanceof DeleteMemberIntent del) {
                desc = "<b>Delete</b> " + getSimpleName(del.getMemberFqn());
                icon = "<font color='#F44336'>[-]</font>";
            } else if (intent instanceof MoveMemberIntent mov) {
                desc = "<b>Move</b> " + getSimpleName(mov.getMemberFqn()) + " " + mov.getPosition() + " " + mov.getAnchorMemberName();
                icon = "<font color='#FF9800'>[M]</font>";
            }
            
            JLabel label = new JLabel("<html>" + icon + " " + desc + "</html>");
            panel.add(label, "inset 0 15 0 0, wrap");
        }
        
        return panel;
    }

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
        return batch;
    }
    
    private String getSimpleName(String fqn) {
        int lastDot = fqn.lastIndexOf('.');
        return lastDot == -1 ? fqn : fqn.substring(lastDot + 1);
    }
}
