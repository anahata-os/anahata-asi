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
 * A specialized lifecycle manager for NetBeans {@link FileObject}-based resources.
 * <p>
 * This helper acts as the "connective tissue" between the IDE's virtual filesystem
 * and the ASI's managed context. It ensures that any changes made by the user 
 * or other IDE tools (like Git or Maven) are immediately propagated to the 
 * model's cached perspective.
 * </p>
 * <p>
 * It is designed for high-fidelity persistence: it handles the restoration of 
 * transient FileObjects and re-attaches listeners after a session is 
 * deserialized from disk.
 * </p>
 * 
 * @author anahata
 */
public class NbFileObjectResourceHelper implements FileChangeListener, Rebindable {

    private static final Logger LOG = Logger.getLogger(NbFileObjectResourceHelper.class.getName());

    /** 
     * The resource that owns this helper. 
     * We ignore this during Jackson serialization to prevent infinite recursion 
     * between the resource and its helper.
     */
    @JsonIgnore
    private AbstractPathResource<?> owner;
    
    /** 
     * The underlying NetBeans FileObject. 
     * Marked transient because FileObjects are live pointers to the IDE's 
     * filesystem and cannot be persisted directly to disk.
     */
    private transient FileObject fileObject;
    
    /** 
     * The weak listener instance. 
     * We use a weak listener to ensure that if the resource is unloaded, 
     * we don't leak memory or keep the FileObject pinned in the IDE's cache.
     */
    private transient FileChangeListener weakListener;

    /**
     * Default constructor required for Kryo's instantiation engine.
     */
    public NbFileObjectResourceHelper() {
    }

    /**
     * Constructs a new helper for a specific resource.
     * 
     * @param owner The managed resource that this helper will keep in sync.
     * @param fo The initial NetBeans FileObject pointer.
     */
    public NbFileObjectResourceHelper(AbstractPathResource<?> owner, FileObject fo) {
        this.owner = owner;
        this.fileObject = fo;
        setupListener();
    }

    /**
     * Ensures the FileObject is live and attached.
     * <p>
     * If the transient FileObject has been lost (e.g., after a session reload), 
     * this method attempts to resolve it using the resource's absolute path.
     * </p>
     * 
     * @return The live FileObject, or null if the file no longer exists on disk.
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
     * Attaches a weak file change listener to the underlying FileObject.
     * This listener is the heart of the ASI's 'LIVE' refresh policy.
     */
    private void setupListener() {
        if (fileObject != null && weakListener == null) {
            this.weakListener = FileUtil.weakFileChangeListener(this, fileObject);
            this.fileObject.addFileChangeListener(weakListener);
        }
    }

    /**
     * Restores the IDE filesystem hooks after a session is restored from disk.
     * <p>
     * This is called by the framework's Rebindable system. It forces the helper 
     * to re-discover its file on the disk and re-register its listeners.
     * </p>
     */
    @Override
    public void rebind() {
        getFileObject();
    }

    /**
     * Performs a clean shutdown of the IDE hooks.
     * Explicitly removes listeners to ensure immediate garbage collection.
     */
    public void dispose() {
        if (fileObject != null && weakListener != null) {
            fileObject.removeFileChangeListener(weakListener);
            this.weakListener = null;
        }
    }

    // --- FileChangeListener Implementation ---

    /** 
     * Triggered when a new file is created. 
     * Not used as we focus on existing resource synchronization.
     * 
     * @param fe The file event.
     */
    @Override public void fileDataCreated(FileEvent fe) {}
    
    /**
     * Triggered when the file content is modified on disk.
     * This invokes the resource's reload logic, ensuring the AI model 
     * always works with the latest 'Ground Truth'.
     * 
     * @param fe The file event.
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

    /**
     * Triggered when the file is deleted from the IDE or filesystem.
     * We log a warning but DO NOT clear the cache, allowing the ASI to 
     * maintain a "ghost" memory of the deleted file for recovery purposes.
     * 
     * @param fe The file event.
     */
    @Override
    public void fileDeleted(FileEvent fe) {
        if (owner != null) {
            LOG.log(Level.WARNING, "File deleted on disk: {0}", owner.getPath());
        }
    }

    /**
     * Triggered when the file is renamed or moved within the filesystem.
     * This is critical for refactoring operations. It updates the ASI's 
     * internal path tracking so the managed resource ID remains valid 
     * even if the file's location changes.
     * 
     * @param fe The rename event.
     */
    @Override
    public void fileRenamed(FileRenameEvent fe) {
        if (owner != null) {
            LOG.log(Level.INFO, "File renamed/moved: {0} -> {1}", new Object[]{owner.getPath(), fe.getFile().getPath()});
            owner.setPath(fe.getFile().getPath());
            owner.setName(fe.getFile().getNameExt());
        }
    }

    /** 
     * Triggered when file attributes change. Not used.
     * 
     * @param fe The attribute event.
     */
    @Override public void fileAttributeChanged(FileAttributeEvent fe) {}
    
    /** 
     * Triggered when a folder is created. Not used.
     * 
     * @param fe The file event.
     */
    @Override public void fileFolderCreated(FileEvent fe) {}
}
