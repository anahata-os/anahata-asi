/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.files.nb;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.openide.cookies.EditorCookie;
import org.openide.cookies.LineCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileSystem;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.model.resource.AbstractPathResource;

/**
 * Stateless utility class containing the core logic for adding and removing 
 * NetBeans FileObjects to/from an AI agi context.
 * <p>
 * It automatically handles both text and binary (multimodal) resources based 
 * on the file's capabilities (cookies) and MIME type.
 * </p>
 * 
 * @author anahata
 */
public class FilesContextActionLogic {

    /** Logger for tracking resource registration and refresh events. */
    private static final Logger LOG = Logger.getLogger(FilesContextActionLogic.class.getName());

    /**
     * Adds a file or folder's contents to the specified agi context.
     * <p>
     * Implementation details:
     * 1. Resolves the FileObject to a physical file.
     * 2. Checks if it's already registered.
     * 3. Uses cookies (Editor/Line) to distinguish between textual and binary resources.
     * 4. Recursively traverses folders if the flag is set.
     * 5. Triggers an IDE UI refresh for the path and its parents.
     * </p>
     * 
     * @param fo The file object to add.
     * @param targetAgi The target agi session.
     * @param recursive Whether to add subfolders recursively.
     */
    public static void addRecursively(FileObject fo, Agi targetAgi, boolean recursive) {
        if (fo.isData()) {
            try {
                File file = FileUtil.toFile(fo);
                if (file != null) {
                    String path = file.getAbsolutePath();
                    if (targetAgi.getResourceManager().findByPath(path).isEmpty()) {
                        DataObject dobj = DataObject.find(fo);
                        AbstractPathResource<?> resource;
                        
                        // Proper NetBeans API check: if it has an editor or line cookie, it's textual.
                        boolean isTextual = dobj.getLookup().lookup(EditorCookie.class) != null 
                                         || dobj.getLookup().lookup(LineCookie.class) != null;

                        if (isTextual) {
                            resource = new NbTextFileResource(targetAgi.getResourceManager(), fo);
                        } else {
                            resource = new NbBinaryFileResource(targetAgi.getResourceManager(), fo);
                            LOG.log(Level.INFO, "Detected binary/multimodal file ({0}): {1}", new Object[]{fo.getMIMEType(), path});
                        }
                        
                        // Set a stable icon ID based on extension. 
                        // The IconProvider will use this as a fallback if live lookup fails.
                        String ext = fo.getExt();
                        resource.setIconId("nb.file." + (ext.isEmpty() ? "unknown" : ext));
                        
                        targetAgi.getResourceManager().register(resource);
                        LOG.log(Level.INFO, "Added file to context of session ''{0}'': {1}", new Object[]{targetAgi.getDisplayName(), path});
                        fireRefreshRecursive(fo);
                    }
                }
            } catch (Exception ex) {
                LOG.log(Level.SEVERE, "Error adding file to context", ex);
            }
        } else if (fo.isFolder()) {
            for (FileObject child : fo.getChildren()) {
                if (child.isData() || recursive) {
                    addRecursively(child, targetAgi, recursive);
                }
            }
        }
    }

    /**
     * Removes a file or folder's contents from the specified agi context.
     * <p>
     * Implementation details:
     * Identifies resources by path and unregisters them from the session. 
     * Triggers an IDE UI refresh.
     * </p>
     * 
     * @param fo The file object to remove.
     * @param targetAgi The target agi session.
     * @param recursive Whether to remove subfolders recursively.
     */
    public static void removeRecursively(FileObject fo, Agi targetAgi, boolean recursive) {
        if (fo.isData()) {
            File file = FileUtil.toFile(fo);
            if (file != null) {
                String path = file.getAbsolutePath();
                targetAgi.getResourceManager().findByPath(path).ifPresent(res -> {
                    targetAgi.getResourceManager().unregister(res.getId());
                    LOG.log(Level.INFO, "Removed file from context of session ''{0}'': {1}", new Object[]{targetAgi.getDisplayName(), path});
                    fireRefreshRecursive(fo);
                });
            }
        } else if (fo.isFolder()) {
            for (FileObject child : fo.getChildren()) {
                if (child.isData() || recursive) {
                    removeRecursively(child, targetAgi, recursive);
                }
            }
        }
    }

    /**
     * Checks if a file is currently in an agi's context.
     * <p>
     * Implementation details:
     * Matches the canonical path of the FileObject against registered resources.
     * </p>
     * 
     * @param fo The file object to check.
     * @param agi The agi session.
     * @return true if registered.
     */
    public static boolean isInContext(FileObject fo, Agi agi) {
        if (fo.isData()) {
            File file = FileUtil.toFile(fo);
            return file != null && agi.getResourceManager().findByPath(file.getAbsolutePath()).isPresent();
        }
        return false;
    }

    /**
     * Counts active agis containing the given file.
     * <p>
     * Implementation details:
     * Iterates through all active sessions and performs a context check.
     * </p>
     * 
     * @param fo The file.
     * @return The agi count.
     */
    public static int countAgisInContext(FileObject fo) {
        int count = 0;
        for (Agi agi : AnahataInstaller.getContainer().getActiveAgis()) {
            if (isInContext(fo, agi)) {
                count++;
            }
        }
        return count;
    }

    /**
     * Returns resource counts for each session within a folder.
     * <p>
     * Implementation details:
     * Builds a sorted map of Agi instances to the number of their resources 
     * located under the given path.
     * </p>
     * 
     * @param fo The target folder.
     * @param recursive Whether to search recursively.
     * @return A sorted map of Agi to count.
     */
    public static Map<Agi, Integer> getSessionFileCounts(FileObject fo, boolean recursive) {
        Map<Agi, Integer> counts = new TreeMap<>((c1, c2) -> c1.getDisplayName().compareTo(c2.getDisplayName()));
        for (Agi agi : AnahataInstaller.getContainer().getActiveAgis()) {
            int count = countFilesInContext(fo, agi, recursive);
            if (count > 0) {
                counts.put(agi, count);
            }
        }
        return counts;
    }

    /**
     * Counts the number of files in context for a specific agi within a folder.
     * <p>
     * Implementation details:
     * Uses prefix-based path matching on the session's resource list to find 
     * children of the given path.
     * </p>
     * 
     * @param fo The file or folder to check.
     * @param agi The agi session.
     * @param recursive Whether to count files in subfolders.
     * @return The file count.
     */
    private static int countFilesInContext(FileObject fo, Agi agi, boolean recursive) {
        File file = FileUtil.toFile(fo);
        if (file == null) {
            return 0;
        }
        String absolutePath = file.getAbsolutePath();

        if (fo.isData()) {
            return agi.getResourceManager().findByPath(absolutePath).isPresent() ? 1 : 0;
        }

        // Folder logic: path-based counting using registered resources
        String folderPrefix = absolutePath.endsWith(File.separator) ? absolutePath : absolutePath + File.separator;

        return (int) agi.getResourceManager().getResources().stream()
                .filter(r -> r instanceof AbstractPathResource)
                .map(r -> (AbstractPathResource<?>) r)
                .filter(r -> {
                    String path = r.getPath();
                    if (!path.startsWith(folderPrefix)) {
                        return false;
                    }
                    if (recursive) {
                        return true;
                    } else {
                        // Non-recursive: must be a direct child (no more separators in remainder)
                        String remainder = path.substring(folderPrefix.length());
                        return !remainder.contains(File.separator);
                    }
                })
                .count();
    }

    /**
     * Fires a comprehensive refresh event for a file and all its ancestors.
     * <p>
     * Implementation details:
     * Ascends the folder tree and aggregates all parents. Dispatches the refresh 
     * event to all active annotation providers to ensure badges and annotations 
     * are redrawn correctly.
     * </p>
     * 
     * @param fo The starting FileObject.
     */
    public static void fireRefreshRecursive(FileObject fo) {
        Map<FileSystem, Set<FileObject>> toRefreshByFs = new HashMap<>();
        FileObject current = fo;
        while (current != null) {
            try {
                FileSystem fs = current.getFileSystem();
                toRefreshByFs.computeIfAbsent(fs, k -> new HashSet<>()).add(current);
            } catch (Exception ex) {
                // Ignore files without a filesystem
            }
            current = current.getParent();
        }
        
        for (Map.Entry<FileSystem, Set<FileObject>> entry : toRefreshByFs.entrySet()) {
            AnahataAnnotationProvider.fireRefresh(entry.getKey(), entry.getValue());
        }
    }
}
