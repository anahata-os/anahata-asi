/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.SwingUtilities;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.openide.DialogDescriptor;
import org.openide.DialogDisplayer;
import uno.anahata.asi.toolkit.files.FileTextReplacements;
import uno.anahata.asi.nb.tools.files.nb.NbFiles;
import uno.anahata.asi.nb.ui.diff.CherryPickDiffPanel;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AnahataToolkit;


/**
 * Advanced coding and refactoring tools for NetBeans.
 * 
 * @author anahata
 */
@Slf4j
@AiToolkit("Advanced coding and refactoring tools for NetBeans.")
public class NbCoding extends AnahataToolkit {

    /** {@inheritDoc} */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return List.of(
                "### NbCoding Toolkit Instructions:\n" +
                "- **NO GUESSING**: You are strictly prohibited from guessing the methods, fields, or signatures of any Java types. " +
                "If a type (like `JavaType` or a library class) is not fully present in your context (including its source code or detailed member list), " +
                "you MUST use `CodeModel.findTypes` or `CodeModel.getTypeSources` to retrieve the ground truth before writing code that uses it."
        );
    }

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
        
        // --- AUDIT VALIDATION ---
        Map<uno.anahata.asi.toolkit.files.TextReplacement, String> validationErrors = new HashMap<>();
        int validCount = 0;
        int totalCount = 0;
        NbFiles nbFiles = getToolkit(NbFiles.class);
        
        for (FileTextReplacements fr : fileReplacements) {
            File f = new File(fr.getPath());
            String fileError = null;
            if (!f.exists()) {
                fileError = "File not found: " + fr.getPath();
            } else if (f.lastModified() != fr.getLastModified()) {
                fileError = "File changed on disk (Stale Context). Disk: " + f.lastModified() + ", Model: " + fr.getLastModified();
            }
            
            String content = null;
            if (fileError == null) {
                try {
                    content = FileUtils.readFileToString(f, StandardCharsets.UTF_8);
                } catch (Exception e) {
                    fileError = "Error reading file: " + e.getMessage();
                }
            }

            for (uno.anahata.asi.toolkit.files.TextReplacement tr : fr.getReplacements()) {
                totalCount++;
                if (fileError != null) {
                    validationErrors.put(tr, fileError);
                } else {
                    int actual = StringUtils.countMatches(content, tr.getTarget());
                    if (tr.getExpectedCount() != -1 && actual != tr.getExpectedCount()) {
                         validationErrors.put(tr, "Occurrence mismatch: Expected " + tr.getExpectedCount() + ", found " + actual);
                    } else {
                        validCount++;
                    }
                }
            }
        }
        
        if (validCount == 0 && totalCount > 0) {
            return "Surgical Safety Check Failed (All suggestions invalid):\n" + validationErrors.values().stream().distinct().collect(java.util.stream.Collectors.joining("\n- ", "- ", ""));
        }
        // --- END AUDIT ---
        
        final CherryPickDiffPanel[] panelHolder = new CherryPickDiffPanel[1];
        SwingUtilities.invokeAndWait(() -> panelHolder[0] = new CherryPickDiffPanel(fileReplacements, validationErrors, this));
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
            
            nbFiles.replaceInMultipleTextFiles(accepted, explanation);
            
            return "Successfully applied changes to " + accepted.size() + " files.";
        } else {
            return "Changes were cancelled by the user. Feedback was preserved.";
        }
    }
}
