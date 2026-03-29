/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.resources.handle;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Paths;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.queries.FileEncodingQuery;
import org.openide.filesystems.FileAttributeEvent;
import org.openide.filesystems.FileChangeListener;
import org.openide.filesystems.FileEvent;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileRenameEvent;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.URLMapper;
import org.openide.loaders.DataObject;
import uno.anahata.asi.internal.TikaUtils;
import uno.anahata.asi.persistence.Rebindable;
import uno.anahata.asi.agi.resource.handle.AbstractResourceHandle;
import uno.anahata.asi.agi.resource.Resource;

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
public class NbHandle extends AbstractResourceHandle implements FileChangeListener, Rebindable {

    /**
     * The unique identifier URI for the resource.
     */
    @NonNull
    @Getter
    private URI uri;

    /**
     * Cached path for secondary resolution.
     */
    @Getter
    private String path;

    /**
     * The live NetBeans FileObject.
     */
    private transient FileObject fileObject;

    /**
     * Weak listener to prevent memory leaks.
     */
    private transient FileChangeListener weakListener;

    /**
     * Constructs a new NbHandle from a URI.
     *
     * @param uri The resource URI (file:, jar:, etc.).
     */
    public NbHandle(URI uri) {
        setUri(uri);
    }

    /**
     * Direct constructor for an existing FileObject.
     *
     * @param fileObject The NetBeans FileObject to wrap.
     */
    public NbHandle(FileObject fileObject) {
        this(fileObject.toURI());
        this.fileObject = fileObject;
        setupListener();
    }

    /**
     * Authoritatively sets and normalizes the resource URI.
     * For local files, it forces the triple-slash (file:///) format and 
     * extracts the physical path to maintain identity consistency within the manager.
     * 
     * @param uri The URI to set.
     */
    private void setUri(URI uri) {
        if (uri != null && uri.getScheme() != null && uri.getScheme().equalsIgnoreCase("file")) {
            this.uri = Paths.get(uri).toUri();
            this.path = this.uri.getPath();
        } else {
            this.uri = uri;
            this.path = null;
        }
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
                    fileObject = FileUtil.toFileObject(FileUtil.normalizeFile(new java.io.File(path)));
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
        return (p != null && !p.isBlank()) ? new java.io.File(p).getName() : uri.toString();
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
                    return TikaUtils.detectMimeType(new java.io.File(path));
                }
            } catch (Exception e) {
                return "application/octet-stream";
            }
        }
        return mime != null ? mime : "application/octet-stream";
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
     * Implementation details: Writes content via the NetBeans FileObject output
     * stream, using the handle's detected charset.</p>
     */
    @Override
    public void write(String content) throws IOException {
        if (!isWritable()) {
            throw new IOException("Resource is read-only: " + uri);
        }

        FileObject fo = getFileObject();
        Charset charset = getCharset();
        try (OutputStream os = fo.getOutputStream()) {
            os.write(content.getBytes(charset));
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
            log.warn("Healing zombie URI on rebind: {}", uri);
            setUri(URI.create(uri.toString()));
        } else {
            setUri(uri);
        }

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
