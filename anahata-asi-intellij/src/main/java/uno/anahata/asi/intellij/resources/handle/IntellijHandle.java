/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.resources.handle;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vcs.FileStatus;
import com.intellij.openapi.vcs.FileStatusManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.openapi.vfs.newvfs.BulkFileListener;
import com.intellij.openapi.vfs.newvfs.events.VFileContentChangeEvent;
import com.intellij.openapi.vfs.newvfs.events.VFileDeleteEvent;
import com.intellij.openapi.vfs.newvfs.events.VFileEvent;
import com.intellij.openapi.vfs.newvfs.events.VFileMoveEvent;
import com.intellij.openapi.vfs.newvfs.events.VFilePropertyChangeEvent;
import com.intellij.util.messages.MessageBusConnection;
import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.List;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.AbstractResourceHandle;
import uno.anahata.asi.internal.TikaUtils;
import uno.anahata.asi.intellij.internal.JavaPsi;
import uno.anahata.asi.persistence.Rebindable;

/**
 * An IntelliJ-native reactive resource handle that wraps physical and virtual
 * files.
 * <p>
 * This handle integrates with the IntelliJ Virtual File System (VFS), listening
 * for file renames, movements, content modifications, and deletions. When a
 * file is moved across packages/folders or renamed, it updates its managed path and URI and marks the
 * parent {@link Resource} dirty so the AI context reflects the change without requiring
 * manual re-adding.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class IntellijHandle extends AbstractResourceHandle implements Rebindable {

    /**
     * The unique identifier URI for the resource.
     */
    @NonNull
    @Getter
    private URI uri;

    /**
     * The absolute filesystem path for local resources, or null if remote.
     */
    @Getter
    private String path;

    /**
     * The live IntelliJ VirtualFile instance.
     */
    private transient VirtualFile virtualFile;


    private transient MessageBusConnection messageBusConnection;
    /**
     * Constructs a new IntelliJ handle from an absolute file path.
     *
     * @param path The absolute path to the local file.
     */
    public IntellijHandle(@NonNull String path) {
        this(Paths.get(path).toUri());
    }

    /**
     * Constructs a new IntelliJ handle from a URI.
     *
     * @param uri The URI of the resource.
     */
    public IntellijHandle(@NonNull URI uri) {
        setUri(uri);
    }

    /**
     * Constructs a new IntelliJ handle directly from a VirtualFile.
     *
     * @param virtualFile The IntelliJ VirtualFile to wrap.
     */
    public IntellijHandle(@NonNull VirtualFile virtualFile) {
        this(Paths.get(virtualFile.getPath()).toUri());
        this.virtualFile = virtualFile;
    }

    /**
     * Sets and normalizes the URI and derived absolute path.
     *
     * @param uri The resource URI.
     */
    private void setUri(URI uri) {
        if (uri != null && uri.getScheme() != null && uri.getScheme().equalsIgnoreCase("file")) {
            this.uri = Paths.get(uri).toUri();
            this.path = Paths.get(this.uri).toAbsolutePath().toString();
        } else {
            this.uri = uri;
            this.path = (uri != null && uri.getPath() != null) ? uri.getPath() : null;
        }
    }

    /**
     * Resolves the live VirtualFile from the VFS cache if not already cached.
     *
     * @return The resolved VirtualFile, or null if unresolved.
     */
    public synchronized VirtualFile getVirtualFile() {
        if (virtualFile == null || !virtualFile.isValid()) {
            if (path != null) {
                virtualFile = JavaPsi.findVirtualFile(path);
            }
            if (virtualFile == null && uri != null) {
                virtualFile = VirtualFileManager.getInstance().findFileByUrl(uri.toString());
            }
        }
        return virtualFile;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Attaches the VFS listener once the owner Resource has been assigned.
     * </p>
     */
    @Override
    public void setOwner(Resource owner) {
        super.setOwner(owner);
        setupListener();
    }

    /**
     * Attaches this handle as a VirtualFileListener to the global
     * VirtualFileManager.
     */
    private synchronized void setupListener() {
        if (messageBusConnection == null) {
            try {
                messageBusConnection = ApplicationManager.getApplication().getMessageBus().connect();
                messageBusConnection.subscribe(VirtualFileManager.VFS_CHANGES, new BulkFileListener() {
                    @Override
                    public void after(@NonNull List<? extends @NonNull VFileEvent> events) {
                        handleVfsEvents(events);
                    }
                });
            } catch (Throwable t) {
                log.warn("Could not register VFS listener for {}: {}", uri, t.getMessage());
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Re-establishes the VFS listener and resolves the VirtualFile after deserialization.
     * </p>
     */
    @Override
    public void rebind() {
        super.rebind();
        if (uri != null && uri.getScheme() == null) {
            setUri(URI.create(uri.toString()));
        } else {
            setUri(uri);
        }
        setupListener();
        getVirtualFile();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Unregisters the VFS listener to prevent memory leaks when the resource is
     * disposed.
     * </p>
     */
    @Override
    public synchronized void dispose() {
        if (messageBusConnection != null) {
            try {
                messageBusConnection.disconnect();
            } catch (Throwable t) {
                log.debug("Error disconnecting VFS listener for {}: {}", uri, t.getMessage());
            }
            messageBusConnection = null;
        }
    }

    private void handleVfsEvents(List<? extends VFileEvent> events) {
        for (VFileEvent event : events) {
            if (event instanceof VFilePropertyChangeEvent propEvent) {
                if (VirtualFile.PROP_NAME.equals(propEvent.getPropertyName())) {
                    VirtualFile eventFile = propEvent.getFile();
                    boolean matches = false;
                    if (virtualFile != null && eventFile.equals(virtualFile)) {
                        matches = true;
                    } else if (path != null) {
                        VirtualFile parent = eventFile.getParent();
                        if (parent != null) {
                            String oldPath = parent.getPath() + "/" + propEvent.getOldValue();
                            if (oldPath.equals(path) || eventFile.getPath().equals(path)) {
                                matches = true;
                            }
                        }
                    }
                    if (matches) {
                        log.info("IntellijHandle detected rename from {} to {}", propEvent.getOldValue(), propEvent.getNewValue());
                        this.virtualFile = eventFile;
                        setUri(Paths.get(eventFile.getPath()).toUri());
                        owner.markDirty();
                    }
                }
            } else if (event instanceof VFileMoveEvent moveEvent) {
                VirtualFile eventFile = moveEvent.getFile();
                boolean matches = false;
                if (virtualFile != null && eventFile.equals(virtualFile)) {
                    matches = true;
                } else if (path != null) {
                    VirtualFile oldParent = moveEvent.getOldParent();
                    if (oldParent != null) {
                        String oldPath = oldParent.getPath() + "/" + eventFile.getName();
                        if (oldPath.equals(path) || eventFile.getPath().equals(path)) {
                            matches = true;
                        }
                    }
                }
                if (matches) {
                    log.info("IntellijHandle detected move for {} from {} to {}", eventFile.getName(),
                            moveEvent.getOldParent().getPath(), moveEvent.getNewParent().getPath());
                    this.virtualFile = eventFile;
                    setUri(Paths.get(eventFile.getPath()).toUri());
                    owner.markDirty();
                }
            } else if (event instanceof VFileContentChangeEvent contentEvent) {
                VirtualFile eventFile = contentEvent.getFile();
                if ((virtualFile != null && eventFile.equals(virtualFile)) || (path != null && eventFile.getPath().equals(path))) {
                    owner.markDirty();
                }
            } else if (event instanceof VFileDeleteEvent deleteEvent) {
                VirtualFile eventFile = deleteEvent.getFile();
                if ((virtualFile != null && eventFile.equals(virtualFile)) || (path != null && eventFile.getPath().equals(path))) {
                    owner.markDirty();
                }
            }
        }
    }
    /**
     * {@inheritDoc}
     */
    @Override
    public String getName() {
        VirtualFile vf = getVirtualFile();
        if (vf != null) {
            return vf.getName();
        }
        return (path != null) ? new File(path).getName() : (uri != null ? uri.toString() : "");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getMimeType() {
        if (path != null) {
            try {
                return TikaUtils.detectMimeType(new File(path));
            } catch (Exception e) {
                return "application/octet-stream";
            }
        }
        return "application/octet-stream";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getLastModified() {
        VirtualFile vf = getVirtualFile();
        if (vf != null && vf.isValid()) {
            return vf.getTimeStamp();
        }
        if (path != null) {
            try {
                return Files.getLastModifiedTime(Paths.get(path)).toMillis();
            } catch (IOException e) {
                return 0;
            }
        }
        return 0;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean exists() {
        VirtualFile vf = getVirtualFile();
        if (vf != null && vf.isValid()) {
            return vf.exists();
        }
        return path != null && Files.exists(Paths.get(path));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InputStream openStream() throws IOException {
        VirtualFile vf = getVirtualFile();
        if (vf != null && vf.isValid()) {
            return vf.getInputStream();
        }
        if (path != null) {
            return Files.newInputStream(Paths.get(path));
        }
        throw new IOException("Resource not found: " + uri);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isWritable() {
        VirtualFile vf = getVirtualFile();
        if (vf != null && vf.isValid()) {
            return vf.isWritable();
        }
        if (path != null) {
            File file = new File(path);
            return file.exists() ? file.canWrite() : (file.getParentFile() != null && file.getParentFile().canWrite());
        }
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void write(String content) throws IOException {
        log.info("Persisting content to local file: {}", path);
        if (path == null) {
            throw new IOException("Cannot write to resource without local path: " + uri);
        }
        Files.writeString(Paths.get(path), content, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
        VirtualFile vf = getVirtualFile();
        if (vf != null) {
            vf.refresh(false, false);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isVirtual() {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long length() {
        VirtualFile vf = getVirtualFile();
        if (vf != null && vf.isValid()) {
            return vf.getLength();
        }
        return path != null ? new File(path).length() : -1L;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Resolves the Version Control System (VCS) status from IntelliJ's {@link FileStatusManager}
     * and wraps the file name in HTML font tags matching the active IDE color scheme (e.g. blue
     * for modified, green for added, grey for ignored). Returns {@code null} if the file has not
     * been modified so standard plain text rendering is used.
     * </p>
     */
    @Override
    public String getHtmlDisplayName() {
        VirtualFile vf = getVirtualFile();
        if (vf == null) {
            return null;
        }
        try {
            return ReadAction.computeBlocking(() -> {
                Project project = JavaPsi.findHostProject(vf);
                if (project == null || project.isDisposed()) {
                    return null;
                }
                FileStatus status = FileStatusManager.getInstance(project).getStatus(vf);
                if (status != null && status != FileStatus.NOT_CHANGED) {
                    Color color = status.getColor();
                    if (color != null) {
                        String hex = String.format("#%02x%02x%02x", color.getRed(), color.getGreen(), color.getBlue());
                        return "<html><font color='" + hex + "'>" + getName() + "</font></html>";
                    }
                }
                return null;
            });
        } catch (Throwable t) {
            log.debug("Could not resolve HTML display name for {}: {}", getPath(), t.getMessage());
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Queries IntelliJ's {@link FileDocumentManager} to determine whether this file currently
     * has unsaved in-memory modifications in an open editor tab.
     * </p>
     */
    @Override
    public boolean isModified() {
        VirtualFile vf = getVirtualFile();
        if (vf != null) {
            try {
                return ReadAction.computeBlocking(() -> FileDocumentManager.getInstance().isFileModified(vf));
            } catch (Throwable t) {
                log.debug("Could not check isFileModified for {}: {}", getPath(), t.getMessage());
            }
        }
        return false;
    }
}
