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

    /** {@inheritDoc} */
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

    /** {@inheritDoc} */
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

    /** {@inheritDoc} */
    @Override
    protected TextFileResource createResourceInstance(String path) throws Exception {
        FileObject fo = FileUtil.toFileObject(new File(path));
        if (fo != null) {
            return new NbTextFileResource(getResourceManager(), fo);
        }
        log("Could not find FileObject for " + path + " loading as normal TextFileResource");
        return super.createResourceInstance(path);
    }

    /** {@inheritDoc} */
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

    /** {@inheritDoc} */
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

    /** {@inheritDoc} */
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
