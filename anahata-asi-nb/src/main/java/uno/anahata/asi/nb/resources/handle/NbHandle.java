/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.resources.handle;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.StyledDocument;
import org.openide.filesystems.FileAlreadyLockedException;
import org.openide.filesystems.FileLock;
import uno.anahata.asi.nb.tools.ide.Editor;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.queries.FileEncodingQuery;
import org.netbeans.modules.localhistory.LocalHistory;
import org.netbeans.modules.localhistory.store.LocalHistoryStore;
import org.netbeans.modules.versioning.core.api.VCSFileProxy;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileAttributeEvent;
import org.openide.filesystems.FileChangeListener;
import org.openide.filesystems.FileEvent;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileRenameEvent;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.URLMapper;
import org.openide.loaders.DataLoaderPool;
import org.openide.loaders.DataObject;
import org.openide.loaders.OperationAdapter;
import org.openide.loaders.OperationEvent;
import org.openide.loaders.OperationListener;
import uno.anahata.asi.internal.TikaUtils;
import uno.anahata.asi.persistence.Rebindable;
import uno.anahata.asi.ide.resources.handle.IdeHandle;

/**
 * A NetBeans-native resource handle that wraps a {@link FileObject}.
 * <p>
 * This is the ultimate handle for the IDE environment. It leverages NetBeans
 * VFS to handle local files, JAR entries, and remote files (via plugins)
 * uniformly. It is reactive: it listens for IDE events and notifies the owner
 * Resource when the content is modified or moved.
 * </p>
 * <p>
 * <b>Archive Awareness:</b> It correctly identifies JAR and Read-Only
 * filesystems to disable writability.
 * </p>
 */
@Slf4j
public class NbHandle extends IdeHandle implements FileChangeListener {

    /**
     * The live NetBeans FileObject.
     */
    private transient FileObject fileObject;

    /**
     * Weak listener to prevent memory leaks.
     */
    private transient FileChangeListener weakListener;

    /**
     * Weak operation listener registered with the IDE's DataLoaderPool to
     * monitor global file moves and lifecycle changes (such as drag-and-drop or
     * cut-and-paste across folders) without creating memory leaks.
     */
    private transient OperationListener weakOpListener;
    /**
     * Strong reference to the operation listener adapter. This field anchors
     * the listener in memory to prevent the JVM's Garbage Collector from
     * reclaiming it while the NbHandle is alive, preserving global
     * file-movement monitoring.
     */
    private transient OperationListener strongOpListener;

    /**
     * Constructs a new NbHandle from a URI.
     *
     * @param uri The resource URI (file:, jar:, etc.).
     */
    public NbHandle(URI uri) {
        super(uri);
        setupOperationListener();
    }

    /**
     * Direct constructor for an existing FileObject.
     *
     * @param fileObject The NetBeans FileObject to wrap.
     */
    public NbHandle(FileObject fileObject) {
        super(fileObject.toURI());
        this.fileObject = fileObject;
        setupListener();
    }

    /**
     * Ensures the FileObject is resolved and listeners are attached.
     *
     * @return The FileObject instance.
     */
    public synchronized FileObject getFileObject() {

        if (fileObject == null || !fileObject.isValid()) {
            try {
                // 1. Try URI resolution (Handles JARs and remote protocols)
                URL url = uri.toURL();
                fileObject = URLMapper.findFileObject(url);

                // 2. Fallback to path resolution for local files
                if (fileObject == null && path != null) {
                    fileObject = FileUtil.toFileObject(FileUtil.normalizeFile(new File(path)));
                }

                if (fileObject != null) {
                    setupListener();
                } else {
                    log.warn("Could not locate fileObject for identity : uri: {} path: {}", uri, path);
                }
            } catch (Exception e) {
                log.error("Failed to resolve FileObject for URI: " + uri, e);
            }
        }
        return fileObject;
    }

    /**
     * Attaches the IDE filesystem listener.
     */
    private void setupListener() {
        if (fileObject != null && weakListener == null) {
            weakListener = FileUtil.weakFileChangeListener(this, fileObject);
            fileObject.addFileChangeListener(weakListener);
        }
        setupOperationListener();
    }

    /**
     * Re-establishes or initializes the global operation listener.
     * <p>
     * Leverages the
     * {@link org.openide.loaders.DataLoaderPool#createWeakOperationListener(org.openide.loaders.OperationListener, java.lang.Object)}
     * utility to bind a weak listener that automatically unloads if this
     * resource handle is GCed.</p>
     */
    private void setupOperationListener() {
        if (strongOpListener == null) {
            strongOpListener = new OperationAdapter() {
                @Override
                public void operationMove(OperationEvent.Move ev) {
                    handleOperationMove(ev);
                }
            };
            weakOpListener = DataLoaderPool.createWeakOperationListener(strongOpListener, DataLoaderPool.getDefault());
            DataLoaderPool.getDefault().addOperationListener(weakOpListener);
        }
    }

    /**
     * Processes global file move operations fired by the IDE loaders layer.
     * <p>
     * If the original primary file of the moved
     * {@link org.openide.loaders.DataObject} matches our active
     * {@link FileObject}, this handle detaches its old file listeners, updates
     * its internally managed URI and path to point to the new destination file,
     * re-attaches new listeners, and marks the parent resource dirty.</p>
     *
     * @param ev The IDE-fired move operation event.
     */
    private void handleOperationMove(OperationEvent.Move ev) {
        FileObject originalFile = ev.getOriginalPrimaryFile();
        boolean matches = false;
        if (originalFile != null) {
            if (originalFile.equals(this.fileObject)) {
                matches = true;
            } else {
                java.net.URI origUri = originalFile.toURI();
                if (origUri.equals(this.uri)) {
                    matches = true;
                } else if (this.path != null && origUri.getPath() != null && origUri.getPath().equals(this.path)) {
                    matches = true;
                }
            }
        }
        if (matches) {
            FileObject newFo = ev.getObject().getPrimaryFile();
            log.info("NbHandle detected move for {} from {} to {}", getName(), originalFile.getPath(), newFo.getPath());
            synchronized (this) {
                if (fileObject != null && weakListener != null) {
                    fileObject.removeFileChangeListener(weakListener);
                    weakListener = null;
                }
                fileObject = newFo;
                setUri(newFo.toURI());
                setupListener();
            }
            if (owner != null) {
                owner.markDirty();
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Resolves name via the NetBeans FileObject or the
     * URI path component.</p>
     */
    @Override
    public String getName() {
        FileObject fo = getFileObject();
        if (fo != null) {
            return fo.getNameExt();
        }
        String p = uri.getPath();
        return (p != null && !p.isBlank()) ? new File(p).getName() : uri.toString();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Extracts the HTML display name from the IDE's
     * DataObject delegate to show Git status or compiler errors.</p>
     */
    @Override
    public String getHtmlDisplayName() {
        FileObject fo = getFileObject();
        if (fo != null) {
            try {
                return DataObject.find(fo).getNodeDelegate().getHtmlDisplayName();
            } catch (Exception e) {
                return null;
            }
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Leverages NetBeans native FileObject MIME
     * detection with a Tika fallback for unrecognized types.</p>
     */
    @Override
    public String getMimeType() {
        FileObject fo = getFileObject();
        String mime = (fo != null) ? fo.getMIMEType() : null;

        if (mime == null || "content/unknown".equals(mime)) {
            try {
                if (path != null) {
                    return TikaUtils.detectMimeType(new File(path));
                }
            } catch (Exception e) {
                return "application/octet-stream";
            }
        }
        return mime != null ? mime : "application/octet-stream";
    }

    /**
     * Currently unsued. Prioritizes the active NetBeans Document content if the
     * file is open in the editor, use this to show unsaved changes to the
     * model.
     *
     * @return The text content of the active document in the editor, or file
     * content if not open.
     * @throws IOException If reading the document or stream fails.
     */
    public String asTextFromEditorIfOpenInEditor() throws IOException {
        FileObject fo = getFileObject();
        if (fo != null) {
            try {
                DataObject dobj = DataObject.find(fo);
                EditorCookie ec = dobj.getLookup().lookup(EditorCookie.class);
                if (ec != null) {
                    Document doc = ec.getDocument();
                    if (doc != null) {
                        final String[] text = new String[1];
                        doc.render(() -> {
                            try {
                                text[0] = doc.getText(0, doc.getLength());
                            } catch (BadLocationException ex) {
                                // Fallback to stream
                            }
                        });
                        if (text[0] != null) {
                            return text[0];
                        }
                    }
                }
            } catch (Exception e) {
                log.warn("Failed to pull live content from editor for {}: {}", getName(), e.getMessage());
            }
        }
        return super.asText();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Retrieves the authoritative last modified
     * timestamp from the NetBeans VFS.</p>
     */
    @Override
    public long getLastModified() {
        FileObject fo = getFileObject();
        return (fo != null) ? fo.lastModified().getTime() : 0;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Checks if the NetBeans DataObject associated with this FileObject
     * is currently marked as modified in the IDE (i.e. has unsaved editor changes).
     * </p>
     */
    @Override
    public boolean isModified() {
        FileObject fo = getFileObject();
        if (fo != null && fo.isValid()) {
            try {
                DataObject dobj = DataObject.find(fo);
                return dobj != null && dobj.isModified();
            } catch (Exception e) {
                log.warn("Failed to check isModified for {}: {}", getName(), e.getMessage());
            }
        }
        return false;
    }



    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Checks the validity of the NetBeans
     * FileObject.</p>
     */
    @Override
    public boolean exists() {
        FileObject fo = getFileObject();
        return fo != null && fo.isValid();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Opens an InputStream directly via the NetBeans
     * FileObject.</p>
     */
    @Override
    public InputStream openStream() throws IOException {
        FileObject fo = getFileObject();
        if (fo == null) {
            throw new IOException("Resource not resolvable in IDE: " + uri);
        }
        return fo.getInputStream();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Checks both filesystem read-only status (e.g.,
     * JARs) and FileObject OS-level writability.</p>
     */
    @Override
    public boolean isWritable() {
        FileObject fo = getFileObject();
        if (fo == null || !fo.isValid()) {
            return false;
        }

        // 1. Check if the filesystem itself is read-only (e.g. JAR, Read-only mount)
        try {
            if (fo.getFileSystem().isReadOnly()) {
                return false;
            }
        } catch (IOException ex) {
            return false;
        }

        // 2. Check if the file object allows writing
        return fo.canWrite();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: If the file is open in an active NetBeans editor tab,
     * updates the live {@link StyledDocument} and saves it through the editor infrastructure
     * to eliminate reload dialogs and lock contention. If not open in an editor, writes
     * via NetBeans FileObject output stream with a retry-guarded lock against background
     * filesystem events, and labels the NetBeans Local History snapshot with the provided reason.
     * </p>
     */
    @Override
    public void write(String content, String reason) throws IOException {
        if (!isWritable()) {
            throw new IOException("Resource is read-only: " + uri);
        }

        FileObject fo = getFileObject();
        EditorCookie ec = Editor.getEditorCookie(fo);

        // 1. If open in an active editor tab, update the live document buffer to prevent lock conflicts
        if (Editor.isFileOpenInEditorTab(ec)) {
            try {
                StyledDocument doc = ec.openDocument();
                if (!doc.getText(0, doc.getLength()).equals(content)) {
                    doc.remove(0, doc.getLength());
                    doc.insertString(0, content, null);
                }
                ec.saveDocument();
            } catch (BadLocationException e) {
                throw new IOException("Failed to update open editor document for " + getName(), e);
            }
        } else {
            // 2. If not open in editor, acquire file lock with retry guard against background events
            Charset charset = getCharset();
            FileLock lock = null;
            for (int i = 0; i < 5; i++) {
                try {
                    lock = fo.lock();
                    break;
                } catch (FileAlreadyLockedException ex) {
                    if (i == 4) {
                        throw ex;
                    }
                    try {
                        Thread.sleep(50);
                    } catch (InterruptedException ie) {
                        Thread.currentThread().interrupt();
                        throw new IOException("Interrupted while waiting for file lock on " + getName(), ie);
                    }
                }
            }

            try (OutputStream os = fo.getOutputStream(lock)) {
                os.write(content.getBytes(charset));
            } finally {
                if (lock != null) {
                    lock.releaseLock();
                }
            }
        }

        if (reason != null && !reason.isBlank()) {
            try {
                File file = FileUtil.toFile(fo);
                if (file != null) {
                    LocalHistoryStore store = LocalHistory.getInstance().getLocalHistoryStore();
                    if (store != null) {
                        VCSFileProxy proxy = VCSFileProxy.createFileProxy(file);
                        long ts = fo.lastModified().getTime();
                        store.setLabel(proxy, ts, reason.trim());
                    }
                }
            } catch (Throwable t) {
                log.error("Could not label Local History for " + getName(), t);
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: returns {@code false} as this represents a
     * physical persistent resource within the IDE.</p>
     */
    @Override
    public boolean isVirtual() {
        return false;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the size of the NetBeans FileObject.</p>
     */
    @Override
    public long length() {
        FileObject fo = getFileObject();
        return (fo != null) ? fo.getSize() : -1L;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Resolves the charset using the NetBeans
     * {@link FileEncodingQuery} to ensure project alignment.</p>
     */
    @Override
    public Charset getCharset() {
        FileObject fo = getFileObject();
        return (fo != null) ? FileEncodingQuery.getEncoding(fo) : super.getCharset();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Refreshes the FileObject resolution and
     * re-attaches listeners after deserialization.</p>
     */
    @Override
    public void rebind() {
        super.rebind();
        // HEALING GUARD: If the deserialized URI is a zombie, force a re-parse.
        if (uri != null && uri.getScheme() == null) {
            log.debug("Healing zombie URI on rebind: {}", uri);
            setUri(URI.create(uri.toString()));
        } else {
            setUri(uri);
        }
        setupOperationListener();
        getFileObject();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Detaches the weak IDE filesystem listener to
     * prevent leaks.</p>
     */
    @Override
    public void dispose() {
        if (fileObject != null && weakListener != null) {
            fileObject.removeFileChangeListener(weakListener);
            weakListener = null;
        }
        if (weakOpListener != null) {
            DataLoaderPool.getDefault().removeOperationListener(weakOpListener);
            weakOpListener = null;
        }
        strongOpListener = null;
    }

    // --- FileChangeListener Implementation ---
    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Notifies the owner resource of content changes
     * detected by the IDE using the new unified {@code markDirty()} marker.</p>
     */
    @Override
    public void fileChanged(FileEvent fe) {
        if (owner != null) {
            owner.markDirty();
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Updates internal URI/Path and notifies owner of
     * the identity change within the IDE using the unified dirty marker.</p>
     */
    @Override
    public void fileRenamed(FileRenameEvent fe) {
        // Path/URI have changed - Re-normalize via the setter
        setUri(fe.getFile().toURI());

        if (owner != null) {
            owner.markDirty();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void fileDataCreated(FileEvent fe) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void fileFolderCreated(FileEvent fe) {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Notifies owner that the source is gone.</p>
     */
    @Override
    public void fileDeleted(FileEvent fe) {
        if (owner != null) {
            owner.markDirty();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void fileAttributeChanged(FileAttributeEvent fe) {
    }
}
