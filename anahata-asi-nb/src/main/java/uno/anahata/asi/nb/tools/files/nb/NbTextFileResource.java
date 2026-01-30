/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.files.nb;

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

    /** The underlying NetBeans FileObject. */
    @Getter
    private final FileObject fileObject;

    /**
     * Constructs a new NbTextFileResource.
     * 
     * @param manager The parent resource manager.
     * @param fo The NetBeans FileObject to wrap.
     * @throws Exception if the initial load fails.
     */
    public NbTextFileResource(ResourceManager manager, FileObject fo) throws Exception {
        super(manager, FileUtil.toFile(fo).toPath());
        this.fileObject = fo;
        // Use a weak listener to avoid memory leaks
        this.fileObject.addFileChangeListener(FileUtil.weakFileChangeListener(this, fileObject));
    }

    /**
     * {@inheritDoc}
     * Implementation details: Uses {@link FileObject#asText()} for efficient 
     * reading and updates the load timestamp from the FileObject metadata.
     */
    @Override
    public void reload() throws Exception {
        if (fileObject == null) {
            // Fallback for initial call from super constructor
            super.reload();
            return;
        }
        
        log.info("Reloading NbTextFileResource: {}", getPath());
        String content = fileObject.asText();
        this.setLoadLastModified(fileObject.lastModified().getTime());
        
        this.getViewport().process(content);
        
        String oldCache = this.cache;
        this.cache = getViewport().getProcessedText();
        propertyChangeSupport.firePropertyChange("cache", oldCache, cache);
    }

    /** {@inheritDoc} */
    @Override
    public long getCurrentLastModified() throws IOException {
        return fileObject.lastModified().getTime();
    }

    /** {@inheritDoc} */
    @Override
    public boolean exists() {
        return fileObject.isValid();
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
