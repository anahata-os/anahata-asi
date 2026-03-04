/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.files.nb.v2;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.charset.Charset;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.queries.FileEncodingQuery;
import org.openide.filesystems.FileAttributeEvent;
import org.openide.filesystems.FileChangeListener;
import org.openide.filesystems.FileEvent;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileRenameEvent;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import uno.anahata.asi.internal.TikaUtils;
import uno.anahata.asi.model.core.Rebindable;
import uno.anahata.asi.resource.v2.AbstractResourceHandle;

/**
 * A NetBeans-native resource handle that wraps a {@link FileObject}.
 * <p>
 * This is the ultimate handle for the IDE environment. It leverages NetBeans VFS 
 * to handle local files, JAR entries, and remote files (via plugins) uniformly. 
 * It is reactive: it listens for IDE events and notifies the owner Resource 
 * when the content is modified or moved.
 * </p>
 */
@Slf4j
@RequiredArgsConstructor
public class NbHandle extends AbstractResourceHandle implements FileChangeListener, Rebindable {

    /** 
     * The path used to resolve the FileObject if it's lost. 
     */
    @NonNull
    private String path;

    /** The live NetBeans FileObject. */
    private transient FileObject fileObject;

    /** Weak listener to prevent memory leaks. */
    private transient FileChangeListener weakListener;

    /**
     * Ensures the FileObject is resolved and listeners are attached.
     * @return The FileObject instance.
     */
    private FileObject getFileObject() {
        if (fileObject == null) {
            fileObject = FileUtil.toFileObject(new java.io.File(path));
            if (fileObject != null) {
                setupListener();
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
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        FileObject fo = getFileObject();
        return (fo != null) ? fo.getNameExt() : new java.io.File(path).getName();
    }

    /** {@inheritDoc} */
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

    /** {@inheritDoc} */
    @Override
    public URI getUri() {
        FileObject fo = getFileObject();
        return (fo != null) ? fo.toURI() : URI.create("file://" + path);
    }

    /** {@inheritDoc} */
    @Override
    public String getMimeType() {
        FileObject fo = getFileObject();
        String mime = (fo != null) ? fo.getMIMEType() : null;
        
        // High-fidelity Fallback: If NB doesn't know, ask Tika
        if (mime == null || "content/unknown".equals(mime)) {
            try {
                return TikaUtils.detectMimeType(new java.io.File(path));
            } catch (Exception e) {
                return "application/octet-stream";
            }
        }
        return mime;
    }

    /** {@inheritDoc} */
    @Override
    public long getLastModified() {
        FileObject fo = getFileObject();
        return (fo != null) ? fo.lastModified().getTime() : 0;
    }

    /** {@inheritDoc} */
    @Override
    public boolean exists() {
        FileObject fo = getFileObject();
        return fo != null && fo.isValid();
    }

    /** {@inheritDoc} */
    @Override
    public InputStream openStream() throws IOException {
        FileObject fo = getFileObject();
        if (fo == null) {
            throw new IOException("FileObject not found: " + path);
        }
        return fo.getInputStream();
    }

    /** {@inheritDoc} */
    @Override
    public boolean isLocal() {
        return true;
    }

    /** {@inheritDoc} */
    @Override
    public Charset getCharset() {
        FileObject fo = getFileObject();
        return (fo != null) ? FileEncodingQuery.getEncoding(fo) : super.getCharset();
    }

    /** {@inheritDoc} */
    @Override
    public void rebind() {
        super.rebind();
        log.debug("Rebinding NbHandle for: {}", path);
        getFileObject();
    }

    /** {@inheritDoc} */
    @Override
    public void dispose() {
        if (fileObject != null && weakListener != null) {
            fileObject.removeFileChangeListener(weakListener);
            weakListener = null;
        }
    }

    // --- FileChangeListener Implementation ---

    /** {@inheritDoc} 
     * Signals the owner resource that the source content has changed.
     */
    @Override
    public void fileChanged(FileEvent fe) {
        if (owner != null) {
            owner.markSourceDirty();
        }
    }

    /** {@inheritDoc} 
     * Updates the path and resource name upon a move/rename operation in the IDE.
     */
    @Override
    public void fileRenamed(FileRenameEvent fe) {
        this.path = fe.getFile().getPath();
        if (owner != null) {
            owner.markSourceDirty();
        }
    }

    /** {@inheritDoc} */
    @Override public void fileDataCreated(FileEvent fe) {}
    /** {@inheritDoc} */
    @Override public void fileFolderCreated(FileEvent fe) {}
    /** {@inheritDoc} */
    @Override public void fileDeleted(FileEvent fe) { if (owner != null) { owner.markSourceDirty(); } }
    /** {@inheritDoc} */
    @Override public void fileAttributeChanged(FileAttributeEvent fe) {}
}
