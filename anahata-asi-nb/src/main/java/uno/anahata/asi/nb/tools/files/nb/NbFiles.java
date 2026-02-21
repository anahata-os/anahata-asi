/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.files.nb;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.text.StyledDocument;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.text.NbDocument;
import org.openide.DialogDescriptor;
import org.netbeans.api.queries.FileEncodingQuery;
import uno.anahata.asi.model.resource.RefreshPolicy;
import uno.anahata.asi.toolkit.files.FileTextReplacements;
import uno.anahata.asi.model.resource.files.TextFileResource;
import uno.anahata.asi.toolkit.files.TextReplacement;
import uno.anahata.asi.toolkit.files.TextViewportSettings;
import uno.anahata.asi.nb.ui.diff.CherryPickDiffPanel;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolException;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.toolkit.files.Files;
import uno.anahata.asi.toolkit.files.TextFileUpdate;

/**
 * A NetBeans-specific implementation of the {@link Files} toolkit.
 * It overrides file modification tools to use NetBeans APIs (FileObject, 
 * DataObject, EditorCookie), ensuring that changes are correctly reflected 
 * in the IDE, trigger local history, and respect file locks.
 * 
 * @author anahata
 */
@AiToolkit("A toolkit for loading and managing file-based resources using NetBeans APIs.")
@Slf4j
public class NbFiles extends Files {

    /** {@inheritDoc} */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        List<String> instructions = new ArrayList<>(super.getSystemInstructions());
        instructions.add(
                "### NbFiles Coding Instructions:\n" +
                "- **NO GUESSING**: You are strictly prohibited from guessing the methods, fields, or signatures of any Java types. " +
                "If a type (like `JavaType` or a library class) is not fully present in your context (including its source code or detailed member list), " +
                "you MUST use `CodeModel.findTypes` or `CodeModel.getTypeSources` to retrieve the ground truth before writing code that uses it.\n" +
                "- **IMPORTS**: Always ensure you add necessary imports when applying changes that include types not already present in the file's import list."
        );
        return instructions;
    }

    /**
     * {@inheritDoc}
     * Implementation details: Overridden to return {@link NbTextFileResource}.
     */
    @Override
    @AiTool(value = "Loads a text file into the context as a managed resource. By default, files are loaded with a LIVE refresh policy, which means they are automatically refreshed from disk right before the API call starts. You do not need to re-load a file if it is already present in the context.")
    public List<TextFileResource> loadTextFile(
            @AiToolParam("The absolute paths to the text files.") List<String> resourcePaths) throws Exception {
        return super.loadTextFile(resourcePaths);
    }

    /**
     * {@inheritDoc}
     * Implementation details: Creates an {@link NbTextFileResource} wrapping 
     * the NetBeans {@link FileObject}.
     */
    @Override
    protected TextFileResource loadTextFile(String path, TextViewportSettings settings) throws Exception {
        
        Optional<TextFileResource> existing = getResourceManager().findByPath(path)
                .filter(r -> r instanceof TextFileResource)
                .map(r -> (TextFileResource) r);
        
        if (existing.isPresent()) {
            TextFileResource resource = existing.get();
            resource.getViewport().setSettings(settings);
            resource.reload();
            log("Updating existing NbTextFileResource: " + path);
            return resource;
        }

        FileObject fo = FileUtil.toFileObject(new File(path));
        if (fo == null) {
            return super.loadTextFile(path, settings);
        }

        NbTextFileResource resource = new NbTextFileResource(getResourceManager(), fo);
        resource.getViewport().setSettings(settings);
        resource.setRefreshPolicy(RefreshPolicy.LIVE);
        resource.reload();
        getResourceManager().register(resource);
        log("Successfully loaded and registered NbTextFileResource: " + resource.getName());
        return resource;
    }

    /**
     * {@inheritDoc}
     * Implementation details: Uses {@link FileUtil#createData} to ensure the 
     * IDE is immediately aware of the new file.
     */
    @Override
    @AiTool(value = "Creates a new file with the provided content.")
    public void createTextFile(
            @AiToolParam("The absolute path to the file.") String path,
            @AiToolParam(value = "The text content to write.", rendererId = "code") String content,
            @AiToolParam("A message describing the change.") String message) throws Exception {
        
        File file = new File(path);
        if (file.exists()) {
            throw new AiToolException("File already exists: " + path);
        }

        File parentFile = file.getParentFile();
        if (parentFile != null && !parentFile.exists()) {
            parentFile.mkdirs();
        }

        FileObject parentFo = FileUtil.toFileObject(parentFile);
        if (parentFo != null) {
            FileObject fo = parentFo.createData(file.getName());
            writeToFileObject(fo, content, message);
            log("Successfully created file via NetBeans API: " + path + " (" + message + ")");
        } else {
            super.createTextFile(path, content, message);
        }
    }

    /**
     * {@inheritDoc}
     * Overwrites an existing file using a rich update object. 
     * Implements optimistic locking and is optimized for the ASI's diff viewer.
     * 
     * @param update The update details (path, content, locking, comments).
     * @param message A message describing the change, used for local history.
     * @throws Exception if the file does not exist, locking fails, or an I/O error occurs.
     */
    @Override
    @AiTool(value = "Overwrites an existing file using a rich update object. Optimized for the ASI's diff viewer.", maxDepth = 12)
    public void updateTextFile(
            @AiToolParam("The update details.") TextFileUpdate update,
            @AiToolParam("A message describing the change, used for local history.") String message) throws Exception {
        
        FileObject fo = FileUtil.toFileObject(new File(update.getPath()));
        if (fo != null) {
            // 1. Optimistic Locking Check
            long current = fo.lastModified().getTime();
            if (update.getLastModified() != 0 && current != update.getLastModified()) {
                throw new AiToolException("Optimistic locking failure: File has been modified in the IDE. Expected: " + update.getLastModified() + ", Actual: " + current);
            }

            writeToFileObject(fo, update.getNewContent(), message);
            log("Successfully updated file via NetBeans API: " + update.getPath() + " (" + message + ")");
        } else {
            super.updateTextFile(update, message);
        }
    }

    /**
     * Helper method to write content to a FileObject, handling both editor 
     * documents and direct stream access.
     * 
     * @param fo The FileObject to write to.
     * @param content The new content.
     * @param message The change message.
     * @throws Exception if the write fails.
     */
    private void writeToFileObject(FileObject fo, String content, String message) throws Exception {
        DataObject dobj = DataObject.find(fo);
        EditorCookie ec = dobj.getLookup().lookup(EditorCookie.class);

        if (ec != null && ec.getOpenedPanes() != null && ec.getOpenedPanes().length > 0) {
            // File is open in editor, use Document API
            log.info("File is open in editor, using Document API for: {}", fo.getPath());
            final StyledDocument doc = ec.openDocument();
            NbDocument.runAtomicAsUser(doc, () -> {
                try {
                    doc.remove(0, doc.getLength());
                    doc.insertString(0, content, null);
                    ec.saveDocument();
                } catch (BadLocationException | IOException ex) {
                    log.error("Error writing to document: " + fo.getPath(), ex);
                }
            });
        } else {
            // Use FileObject directly with Lock
            log.info("Using FileObject API with lock for: {}", fo.getPath());
            FileLock lock = fo.lock();
            try (OutputStream os = fo.getOutputStream(lock)) {
                Charset encoding = FileEncodingQuery.getEncoding(fo);
                os.write(content.getBytes(encoding));
            } finally {
                lock.releaseLock();
            }
        }
    }

    /**
     * {@inheritDoc}
     * Implementation details: Delegates to {@link #updateTextFile} after 
     * performing the string replacements on the current content.
     */
    @Override
    @AiTool(value = "Performs multiple text replacements in a file. Ideal for surgical code edits.")
    public void replaceInTextFile(
            @AiToolParam("The absolute path to the file.") String path,
            @AiToolParam("The list of replacements to perform.") List<TextReplacement> replacements, 
            @AiToolParam("Optimistic locking: the expected last modified timestamp of the file on disk.") long lastModified,
            @AiToolParam("A message describing the change, used for local history.") String message) throws Exception {
        
        FileObject fo = FileUtil.toFileObject(new File(path));
        if (fo != null) {
            String content = fo.asText();
            String newContent = performReplacements(content, replacements);
            updateTextFile(TextFileUpdate.builder()
                    .path(path)
                    .newContent(newContent)
                    .lastModified(lastModified)
                    .build(), message);
        } else {
            super.replaceInTextFile(path, replacements, lastModified, message);
        }
    }

    @Override
    @AiTool(value = "Performs multiple text replacements across multiple files in a single tool call. "
            + "This tool proposes surgical changes with a cherry-picking diff viewer for user review.")
    public void replaceInMultipleTextFiles(
            @AiToolParam("The list of files and their replacements.") List<FileTextReplacements> fileReplacements,
            @AiToolParam("A message describing the change.") String message) throws Exception {
        
        // --- AUDIT VALIDATION ---
        Map<TextReplacement, String> validationErrors = new HashMap<>();
        int validCount = 0;
        int totalCount = 0;
        
        for (FileTextReplacements fr : fileReplacements) {
            File f = new File(fr.getPath());
            String fileError = null;
            if (!f.exists()) {
                fileError = "File not found: " + fr.getPath();
            } else {
                FileObject fo = FileUtil.toFileObject(f);
                if (fo != null && fr.getLastModified() != 0 && fo.lastModified().getTime() != fr.getLastModified()) {
                    fileError = "File changed on disk (Stale Context). Disk: " + fo.lastModified().getTime() + ", Model: " + fr.getLastModified();
                }
            }
            
            String content = null;
            if (fileError == null) {
                try {
                    content = FileUtils.readFileToString(f, StandardCharsets.UTF_8);
                } catch (Exception e) {
                    fileError = "Error reading file: " + e.getMessage();
                }
            }

            for (TextReplacement tr : fr.getReplacements()) {
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
            error("Surgical Safety Check Failed (All suggestions invalid):\n" + validationErrors.values().stream().distinct().collect(java.util.stream.Collectors.joining("\n- ", "- ", "")));
            return;
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
                log("No changes were selected by the user.");
                return;
            }
            
            for (FileTextReplacements fr : accepted) {
                replaceInTextFile(fr.getPath(), fr.getReplacements(), fr.getLastModified(), message);
            }
            
            log("Successfully applied changes to " + accepted.size() + " files.");
        } else {
            log("Changes were cancelled by the user. Feedback was preserved.");
        }
    }
}
