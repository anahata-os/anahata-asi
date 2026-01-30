/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.files.nb;

import java.io.File;
import java.io.IOException;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.openide.filesystems.FileAttributeEvent;
import org.openide.filesystems.FileChangeListener;
import org.openide.filesystems.FileEvent;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileRenameEvent;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.model.resource.TextFileResource;
import uno.anahata.asi.resource.ResourceManager;

/**
 * A NetBeans-specific implementation of {@link TextFileResource} that wraps a 
 * {@link FileObject} and listens for filesystem changes using the NetBeans 
 * FileSystem API.
 * <p>
 * This resource ensures that the AI context remains in sync with the IDE's 
 * state, including renames and external modifications.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class NbTextFileResource extends TextFileResource implements FileChangeListener {

    /** The underlying NetBeans FileObject. Transient to support serialization. */
    private transient FileObject fileObject;
    
    /** The weak listener instance, stored to allow explicit removal. */
    private transient FileChangeListener weakListener;

    /**
     * Constructs a new NbTextFileResource.
     * 
     * @param manager The parent resource manager.
     * @param fo The NetBeans FileObject to wrap.
     * @throws Exception if the initial setup fails.
     */
    public NbTextFileResource(ResourceManager manager, FileObject fo) throws Exception {
        super(manager, FileUtil.toFile(fo).toPath());
        this.fileObject = fo;
        setupListener();
    }

    /**
     * Ensures the FileObject is available, restoring it from the path if necessary.
     * 
     * @return The FileObject, or null if it cannot be restored.
     */
    private FileObject ensureFileObject() {
        if (fileObject == null && getPath() != null) {
            log.info("Restoring FileObject for NbTextFileResource: {}", getPath());
            this.fileObject = FileUtil.toFileObject(new File(getPath()));
            if (this.fileObject != null) {
                setupListener();
            } else {
                log.error("Failed to restore FileObject for path: {}", getPath());
            }
        }
        return fileObject;
    }

    /**
     * Attaches a weak file change listener to the FileObject.
     */
    private void setupListener() {
        if (fileObject != null && weakListener == null) {
            this.weakListener = FileUtil.weakFileChangeListener(this, fileObject);
            this.fileObject.addFileChangeListener(weakListener);
        }
    }

    /**
     * {@inheritDoc}
     * Implementation details: Uses {@link FileObject#asText()} for efficient 
     * reading and updates the load timestamp from the FileObject metadata.
     */
    @Override
    public void reload() throws Exception {
        FileObject fo = ensureFileObject();
        if (fo == null) {
            throw new IOException("FileObject not available for: " + getPath());
        }
        
        log.info("Reloading NbTextFileResource: {}", getPath());
        String content = fo.asText();
        this.setLoadLastModified(fo.lastModified().getTime());
        
        this.getViewport().process(content);
        
        String oldCache = this.cache;
        this.cache = getViewport().getProcessedText();
        propertyChangeSupport.firePropertyChange("cache", oldCache, cache);
    }

    /** {@inheritDoc} */
    @Override
    public long getCurrentLastModified() throws IOException {
        FileObject fo = ensureFileObject();
        return fo != null ? fo.lastModified().getTime() : 0L;
    }

    /** {@inheritDoc} */
    @Override
    public boolean exists() {
        FileObject fo = ensureFileObject();
        return fo != null && fo.isValid();
    }

    /** {@inheritDoc} */
    @Override
    public void rebind() {
        super.rebind();
        ensureFileObject();
    }

    /** {@inheritDoc} */
    @Override
    public void dispose() {
        super.dispose();
        if (fileObject != null && weakListener != null) {
            fileObject.removeFileChangeListener(weakListener);
            this.weakListener = null;
        }
    }

    // --- FileChangeListener Implementation ---

    /** {@inheritDoc} */
    @Override
    public void fileDataCreated(FileEvent fe) {
        // Not applicable for an existing resource
    }

    /** {@inheritDoc} */
    @Override
    public void fileChanged(FileEvent fe) {
        log.info("File changed on disk, triggering reload: {}", getPath());
        try {
            reload();
        } catch (Exception e) {
            log.error("Failed to reload NbTextFileResource on change", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void fileDeleted(FileEvent fe) {
        log.warn("File deleted on disk: {}", getPath());
        // The resource remains in context but exists() will return false
    }

    /** {@inheritDoc} */
    @Override
    public void fileRenamed(FileRenameEvent fe) {
        log.info("File renamed: {} -> {}", getPath(), fe.getFile().getPath());
        this.setPath(fe.getFile().getPath());
        this.setName(fe.getFile().getNameExt());
    }

    /** {@inheritDoc} */
    @Override
    public void fileAttributeChanged(FileAttributeEvent fe) {
        // Ignore attribute changes
    }

    /** {@inheritDoc} */
    @Override
    public void fileFolderCreated(FileEvent fe) {
        // Not applicable
    }
}
