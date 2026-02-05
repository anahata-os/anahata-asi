/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.files.nb;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.List;
import javax.swing.text.BadLocationException;
import javax.swing.text.StyledDocument;
import lombok.extern.slf4j.Slf4j;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.text.NbDocument;
import org.netbeans.api.queries.FileEncodingQuery;
import uno.anahata.asi.model.context.RefreshPolicy;
import uno.anahata.asi.model.resource.FileTextReplacements;
import uno.anahata.asi.model.resource.TextFileResource;
import uno.anahata.asi.model.resource.TextReplacement;
import uno.anahata.asi.model.resource.TextViewportSettings;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolException;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.toolkit.files.Files;

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

    /**
     * {@inheritDoc}
     * Implementation details: Overridden to return {@link NbTextFileResource}.
     */
    @Override
    @AiTool(value = "Loads a text file into the context as a managed resource. By default, files are loaded with a LIVE refresh policy, which means they are automatically refreshed from disk right before the API call starts. You do not need to re-load a file if it is already present in the context.", retention = 0)
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
        if (getResourceManager().findByPath(path).isPresent()) {
            throw new AiToolException("Resource already loaded for path: " + path);
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
        log("Successfully loaded and registered NbTextFileResource: " + path);
        return resource;
    }

    /**
     * {@inheritDoc}
     * Implementation details: Uses {@link FileUtil#createData} to ensure the 
     * IDE is immediately aware of the new file.
     */
    @Override
    @AiTool(value = "Creates a new file with the provided content.", retention = 0)
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
     * Implementation details: Uses {@link EditorCookie} to write to the document 
     * if the file is open in the editor, otherwise uses {@link FileObject#getOutputStream()} 
     * with a proper {@link FileLock}.
     */
    @Override
    @AiTool(value = "Overwrites an existing file with the provided content.", retention = 0)
    public void updateTextFile(
            @AiToolParam("The absolute path to the file.") String path,
            @AiToolParam(value = "The text content to write.", rendererId = "code") String content,
            @AiToolParam("Optimistic locking: the expected last modified timestamp of the file on disk.") long lastModified,
            @AiToolParam("A message describing the change, used for local history.") String message) throws Exception {
        
        FileObject fo = FileUtil.toFileObject(new File(path));
        if (fo != null) {
            // 1. Optimistic Locking Check
            long current = fo.lastModified().getTime();
            if (lastModified != 0 && current != lastModified) {
                throw new AiToolException("Optimistic locking failure: File has been modified in the IDE. Expected: " + lastModified + ", Actual: " + current);
            }

            writeToFileObject(fo, content, message);
            log("Successfully updated file via NetBeans API: " + path + " (" + message + ")");
        } else {
            super.updateTextFile(path, content, lastModified, message);
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
    @AiTool(value = "Performs multiple text replacements in a file. Ideal for surgical code edits.", retention = 0)
    public void replaceInTextFile(
            @AiToolParam("The absolute path to the file.") String path,
            @AiToolParam("The list of replacements to perform.") List<TextReplacement> replacements, 
            @AiToolParam("Optimistic locking: the expected last modified timestamp of the file on disk.") long lastModified,
            @AiToolParam("A message describing the change, used for local history.") String message) throws Exception {
        
        FileObject fo = FileUtil.toFileObject(new File(path));
        if (fo != null) {
            String content = fo.asText();
            String newContent = performReplacements(content, replacements);
            updateTextFile(path, newContent, lastModified, message);
        } else {
            super.replaceInTextFile(path, replacements, lastModified, message);
        }
    }

    /**
     * {@inheritDoc}
     * Implementation details: Overridden to ensure NetBeans-aware replacements 
     * are used for each file.
     */
    @Override
    @AiTool(value = "Performs multiple text replacements across multiple files in a single tool call.", retention = 0)
    public void replaceInMultipleTextFiles(
            @AiToolParam("The list of files and their replacements.") List<FileTextReplacements> fileReplacements,
            @AiToolParam("A message describing the change.") String message) throws Exception {
        for (FileTextReplacements fr : fileReplacements) {
            replaceInTextFile(fr.getPath(), fr.getReplacements(), fr.getLastModified(), message);
        }
    }
}
