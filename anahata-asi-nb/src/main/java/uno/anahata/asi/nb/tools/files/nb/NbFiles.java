/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.files.nb;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.text.NbDocument;
import org.netbeans.api.queries.FileEncodingQuery;
import uno.anahata.asi.model.resource.RefreshPolicy;
import uno.anahata.asi.model.resource.files.TextFileResource;
import uno.anahata.asi.toolkit.files.TextViewportSettings;
import uno.anahata.asi.tool.AiToolException;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.toolkit.files.Files;
import uno.anahata.asi.toolkit.files.FullTextFileUpdate;

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
     * Returns the mandatory system instructions for this toolkit.
     * <p>
     * Implementation details:
     * Appends a strict prohibition against guessing Java type signatures.
     * </p>
     * 
     * @return List of instructions.
     * @throws Exception on error.
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        List<String> instructions = new ArrayList<>(super.getSystemInstructions());
        instructions.add(
                "The NbFiles Toolkit :\n" +
                "- You are strictly prohibited from guessing the methods, fields, or signatures of the Java types in the users projects. " +
                "If a given type in the users projects is not present in your context (like the source code or members list), " +
                "then use `CodeModel` tools or `loadTextFile` to retrieve the ground truth before writing code that uses it.\n" +
                "Avoid `replaceTextInFile` for coding."
        );
        return instructions;
    }

    /**
     * Loads a text file as a managed resource.
     * <p>
     * Implementation details:
     * Overrides the core logic to trigger an IDE UI refresh for the file's icon 
     * and status immediately after registration.
     * </p>
     * 
     * @param path Absolute path.
     * @param settings Viewport settings.
     * @return The loaded resource.
     * @throws Exception on failure.
     */
    @Override
    public TextFileResource loadTextFileInternal(String path, TextViewportSettings settings) throws Exception {
        TextFileResource resource = super.loadTextFileInternal(path, settings);
        FileObject fo = FileUtil.toFileObject(new File(path));
        if (fo != null) {
            FilesContextActionLogic.fireRefreshRecursive(fo);
        }
        return resource;
    }

    /**
     * Updates viewport settings for an already loaded resource.
     * <p>
     * Implementation details:
     * Manages logic for re-loading resources based on refresh policies.
     * </p>
     * 
     * @param resource The resource to update.
     * @param settings The new settings.
     * @throws Exception on error.
     */
    @Override
    protected void updateExistingResource(TextFileResource resource, TextViewportSettings settings) throws Exception {
        if (settings != null) {
            log("File was already loaded, Updating viewport settings for: " + resource.getPath());
            resource.getViewport().setSettings(settings);
        }
        
        if (resource.getRefreshPolicy() == RefreshPolicy.SNAPSHOT) {
            log("Updating existing NbTextFileResource: " + resource.getPath());
            resource.reload();
        } else if (settings == null) {
            error("No point in reloading a file that is already in context with LIVE refresh policy: " + resource.getPath());
        }
    }

    /**
     * Creates a new resource instance for the given path.
     * <p>
     * Implementation details:
     * Returns a {@link NbTextFileResource} which integrates with the NetBeans 
     * FileObject lifecycle.
     * </p>
     * 
     * @param path Absolute path.
     * @return The resource instance.
     * @throws Exception on creation error.
     */
    @Override
    protected TextFileResource createResourceInstance(String path) throws Exception {
        FileObject fo = FileUtil.toFileObject(new File(path));
        if (fo != null) {
            return new NbTextFileResource(getResourceManager(), fo);
        }
        log("Could not find FileObject for " + path + " loading as normal TextFileResource");
        return super.createResourceInstance(path);
    }

    /**
     * Creates a new file on disk.
     * <p>
     * Implementation details:
     * Uses the NetBeans {@link FileObject#createData} API to ensure the IDE 
     * indexes the new file immediately. Triggers a UI refresh.
     * </p>
     * 
     * @param path Absolute path.
     * @param content Content.
     * @param message Change message.
     * @throws Exception on write error.
     */
    @Override
    protected void performCreate(String path, String content, String message) throws Exception {
        File file = new File(path);
        FileObject parentFo = FileUtil.toFileObject(file.getParentFile());
        if (parentFo != null) {
            FileObject fo = parentFo.createData(file.getName());
            writeToFileObject(fo, content, message);
            log("Successfully created file via NetBeans API: " + path + " (" + message + ")");
            // Auto-load newly created file
            loadTextFileInternal(path, new TextViewportSettings());
        } else {
            super.performCreate(path, content, message);
        }
    }

    /**
     * Validates an update request against the managed resource.
     * <p>
     * Implementation details:
     * Performs optimistic locking by comparing timestamps and ensures the path 
     * is a managed resource.
     * </p>
     * 
     * @param update The update details.
     * @throws Exception if validation fails.
     */
    @Override
    protected void validateUpdate(FullTextFileUpdate update) throws Exception {
        FileObject fo = FileUtil.toFileObject(new File(update.getPath()));
        if (fo != null) {
            // Context Check
            getResourceManager().findByPath(update.getPath())
                .filter(r -> r instanceof TextFileResource)
                .orElseThrow(() -> new AiToolException("Update rejected: '" + update.getPath() + "' is not a managed resource."));

            // Optimistic Locking Check
            long current = fo.lastModified().getTime();
            if (update.getLastModified() != 0 && current != update.getLastModified()) {
                throw new AiToolException("Optimistic locking failure: File modified in IDE. Expected: " + update.getLastModified() + ", Actual: " + current);
            }
        } else {
            super.validateUpdate(update);
        }
    }

    /**
     * Performs a file update.
     * <p>
     * Implementation details:
     * Uses NetBeans APIs to perform a synchronized write to the FileObject.
     * </p>
     * 
     * @param update The update.
     * @param message Message.
     * @throws Exception on write error.
     */
    @Override
    protected void performUpdate(FullTextFileUpdate update, String message) throws Exception {
        FileObject fo = FileUtil.toFileObject(new File(update.getPath()));
        if (fo != null) {
            writeToFileObject(fo, update.getNewContent(), message);
            log("Successfully updated file via NetBeans API: " + update.getPath() + " (" + message + ")");
        } else {
            super.performUpdate(update, message);
        }
    }

    /**
     * Helper method to write content to a FileObject, handling both editor 
     * documents and direct stream access.
     * <p>
     * Implementation details:
     * If the file is open in an editor, it uses {@link NbDocument#runAtomicAsUser} 
     * to update the document, preserving undo history. Otherwise, it uses 
     * {@link FileObject#lock()} for a safe stream write.
     * </p>
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
            log.info("File is open in editor, using Document API for: {}", fo.getPath());
            final javax.swing.text.StyledDocument doc = ec.openDocument();
            NbDocument.runAtomicAsUser(doc, () -> {
                try {
                    doc.remove(0, doc.getLength());
                    doc.insertString(0, content, null);
                    ec.saveDocument();
                } catch (javax.swing.text.BadLocationException | IOException ex) {
                    log.error("Error writing to document: " + fo.getPath(), ex);
                }
            });
        } else {
            log.info("Using FileObject API with lock for: {}", fo.getPath());
            Charset encoding = FileEncodingQuery.getEncoding(fo);
            FileLock lock = fo.lock();
            try {
                try (OutputStream os = fo.getOutputStream(lock)) {
                    os.write(content.getBytes(encoding));
                }
            } finally {
                lock.releaseLock();
            }
        }
    }
}
