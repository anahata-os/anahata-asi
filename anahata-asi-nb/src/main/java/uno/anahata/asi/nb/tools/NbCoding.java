/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools;

import java.util.List;
import javax.swing.SwingUtilities;
import lombok.extern.slf4j.Slf4j;
import org.openide.DialogDescriptor;
import org.openide.DialogDisplayer;
import uno.anahata.asi.model.resource.FileTextReplacements;
import uno.anahata.asi.nb.tools.files.nb.NbFiles;
import uno.anahata.asi.nb.ui.diff.CherryPickDiffPanel;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.ToolContext;

/**
 * Advanced coding and refactoring tools for NetBeans.
 * 
 * @author anahata
 */
@Slf4j
@AiToolkit("Advanced coding and refactoring tools for NetBeans.")
public class NbCoding extends ToolContext {

    /**
     * Proposes surgical text replacements across multiple files with a cherry-picking diff viewer.
     * 
     * @param fileReplacements The list of files and their proposed replacements.
     * @param explanation A clear explanation of the changes.
     * @return A summary of the operation.
     * @throws Exception if the operation fails.
     */
    @AiTool("Proposes surgical text replacements across multiple files with a cherry-picking diff viewer. This is the preferred tool for applying complex code changes.")
    public String suggestSurgicalChanges(
            @AiToolParam("The list of files and their proposed replacements.") List<FileTextReplacements> fileReplacements, 
            @AiToolParam("A clear explanation of the changes.") String explanation) throws Exception {
        
        final CherryPickDiffPanel[] panelHolder = new CherryPickDiffPanel[1];
        SwingUtilities.invokeAndWait(() -> panelHolder[0] = new CherryPickDiffPanel(fileReplacements, this));
        CherryPickDiffPanel panel = panelHolder[0];
        
        DialogDescriptor dd = new DialogDescriptor(panel, "Review Surgical Changes");
        dd.setHelpCtx(null);
        panel.setupDialog(dd);
        
        java.awt.Dialog dialog = org.openide.DialogDisplayer.getDefault().createDialog(dd);
        dialog.setResizable(true);
        dialog.setBounds(java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds());
        dialog.setVisible(true);
        
        List<FileTextReplacements> accepted = panel.getAcceptedReplacements();
        String comments = panel.getAggregatedComments();
        
        if (!comments.isEmpty()) {
            log("User provided feedback during cherry-picking:\n" + comments);
            getResponse().setUserFeedback(comments);
        }
        
        // Attach any captured screenshots
        List<byte[]> screenshots = panel.getScreenshots();
        if (!screenshots.isEmpty()) {
            log("Attaching " + screenshots.size() + " screenshot(s) captured in the diff viewer.");
            for (byte[] screenshot : screenshots) {
                getResponse().addAttachment(screenshot, "image/png");
            }
        }

        if (dd.getValue() == panel.getOkBtn()) {
            if (accepted.isEmpty()) {
                return "No changes were selected by the user.";
            }
            
            NbFiles nbFiles = getToolkit(NbFiles.class);
            nbFiles.replaceInMultipleTextFiles(accepted, explanation);
            
            return "Successfully applied changes to " + accepted.size() + " files.";
        } else {
            return "Changes were cancelled by the user. Feedback was preserved.";
        }
    }
}
