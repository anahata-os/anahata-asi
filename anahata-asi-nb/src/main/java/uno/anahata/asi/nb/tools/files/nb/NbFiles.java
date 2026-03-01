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
import uno.anahata.asi.toolkit.files.AbstractTextFileWrite;
import uno.anahata.asi.toolkit.files.FullTextFileCreate;
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
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Appends a strict prohibition against guessing Java type signatures.
     * </p>
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
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Overrides the core logic to trigger an IDE UI refresh for the file's icon 
     * and status immediately after registration.
     * </p>
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
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Manages logic for re-loading resources based on refresh policies.
     * </p>
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
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Returns a {@link NbTextFileResource} which integrates with the NetBeans 
     * FileObject lifecycle.
     * </p>
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
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Uses {@link FileObject#createData} to ensure immediate indexing and triggers
     * a UI refresh.
     * </p>
     */
    @Override
    protected void performCreate(FullTextFileCreate create, String message) throws Exception {
        File file = new File(create.getPath());
        FileObject parentFo = FileUtil.toFileObject(file.getParentFile());
        if (parentFo != null) {
            FileObject fo = parentFo.createData(file.getName());
            writeToFileObject(fo, create.getContent(), message);
            log("Successfully created file via NetBeans API: " + create.getPath() + " (" + message + ")");
            // Auto-load newly created file
            loadTextFileInternal(create.getPath(), new TextViewportSettings());
        } else {
            super.performCreate(create, message);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Performs optimistic locking by comparing timestamps using the NetBeans 
     * FileObject metadata and ensures the path is a managed resource.
     * </p>
     */
    @Override
    public void validateWrite(AbstractTextFileWrite write) throws Exception {
        FileObject fo = FileUtil.toFileObject(new File(write.getPath()));
        if (fo != null) {
            // Context Check
            getResourceManager().findByPath(write.getPath())
                .filter(r -> r instanceof TextFileResource)
                .orElseThrow(() -> new AiToolException("Update rejected: '" + write.getPath() + "' is not a managed resource."));

            // Optimistic Locking Check
            long current = fo.lastModified().getTime();
            if (write.getLastModified() != 0 && current != write.getLastModified()) {
                throw new AiToolException("Optimistic locking failure: You gave: " + write.getLastModified() + ", Actual: " + current);
            }
        } else {
            super.validateWrite(write);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Uses NetBeans APIs to perform a synchronized write to the FileObject.
     * </p>
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
