/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.files.nb;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.openide.filesystems.FileAttributeEvent;
import org.openide.filesystems.FileChangeListener;
import org.openide.filesystems.FileEvent;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileRenameEvent;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.model.core.Rebindable;
import uno.anahata.asi.model.resource.AbstractPathResource;

/**
 * A helper class that manages the lifecycle and event listening for NetBeans 
 * {@link FileObject}-based resources.
 * <p>
 * It implements {@link Rebindable} to handle restoration of transient 
 * FileObjects and weak listeners after serialization. It avoids using 
 * Lambdas to prevent Kryo "hidden class" serialization errors.
 * </p>
 * 
 * @author anahata
 */
public class NbFileObjectResourceHelper implements FileChangeListener, Rebindable {

    private static final Logger LOG = Logger.getLogger(NbFileObjectResourceHelper.class.getName());

    /** 
     * The resource that owns this helper. 
     * Ignored by Jackson to prevent circular serialization loops.
     */
    @JsonIgnore
    private AbstractPathResource<?, ?> owner;
    
    /** The underlying NetBeans FileObject. Transient to support serialization. */
    private transient FileObject fileObject;
    
    /** The weak listener instance. */
    private transient FileChangeListener weakListener;

    /**
     * Default constructor required for Kryo serialization.
     */
    public NbFileObjectResourceHelper() {
    }

    /**
     * Constructs a new helper for a specific resource.
     * 
     * @param owner The owning resource.
     * @param fo The initial FileObject.
     */
    public NbFileObjectResourceHelper(AbstractPathResource<?, ?> owner, FileObject fo) {
        this.owner = owner;
        this.fileObject = fo;
        setupListener();
    }

    /**
     * Ensures the FileObject is available, restoring it from the path if necessary.
     * 
     * @return The FileObject, or null if it cannot be restored.
     */
    public FileObject getFileObject() {
        if (fileObject == null && owner != null && owner.getPath() != null) {
            this.fileObject = FileUtil.toFileObject(new File(owner.getPath()));
            if (this.fileObject != null) {
                setupListener();
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
     * Restores the FileObject and listeners after deserialization.
     */
    @Override
    public void rebind() {
        getFileObject();
    }

    /**
     * Disposes of the helper and removes listeners.
     */
    public void dispose() {
        if (fileObject != null && weakListener != null) {
            fileObject.removeFileChangeListener(weakListener);
            this.weakListener = null;
        }
    }

    // --- FileChangeListener Implementation ---

    /** {@inheritDoc} */
    @Override public void fileDataCreated(FileEvent fe) {}
    
    /**
     * {@inheritDoc}
     * Triggers a reload of the owning resource when the file changes on disk.
     */
    @Override
    public void fileChanged(FileEvent fe) {
        if (owner != null) {
            try {
                owner.reload();
            } catch (Exception e) {
                LOG.log(Level.SEVERE, "Failed to reload resource on change: " + owner.getPath(), e);
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void fileDeleted(FileEvent fe) {
        if (owner != null) {
            LOG.log(Level.WARNING, "File deleted on disk: {0}", owner.getPath());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void fileRenamed(FileRenameEvent fe) {
        if (owner != null) {
            LOG.log(Level.INFO, "File renamed: {0} -> {1}", new Object[]{owner.getPath(), fe.getFile().getPath()});
            owner.setPath(fe.getFile().getPath());
            owner.setName(fe.getFile().getNameExt());
        }
    }

    /** {@inheritDoc} */
    @Override public void fileAttributeChanged(FileAttributeEvent fe) {}
    /** {@inheritDoc} */
    @Override public void fileFolderCreated(FileEvent fe) {}
}
