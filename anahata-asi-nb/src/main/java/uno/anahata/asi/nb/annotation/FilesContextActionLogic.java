/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.annotation;

import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.logging.Logger;
import lombok.extern.slf4j.Slf4j;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileSystem;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.nb.AnahataInstaller;
import uno.anahata.asi.agi.Agi;

/**
 * Logic handler for adding NetBeans files and folders to the V2 AI context.
 * <p>
 * This class bridges the IDE's {@link FileObject} selections with the 
 * {@link uno.anahata.asi.agi.resource.ResourceManager}. It handles recursion 
 * for folders and provides a V1-compatible API for seamless migration.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class FilesContextActionLogic {

    private static final Logger LOG = Logger.getLogger(FilesContextActionLogic.class.getName());

    /**
     * Adds a file or folder's contents to the specified agi context recursively.
     * 
     * @param fo The starting FileObject.
     * @param targetAgi The target agi session.
     * @param recursive True to traverse subfolders.
     */
    public static void addRecursively(FileObject fo, Agi targetAgi, boolean recursive) {
        List<Path> pathsToRegister = new ArrayList<>();
        Set<FileObject> fosToRefresh = new HashSet<>();
        
        collectAdditions(fo, targetAgi, recursive, pathsToRegister, fosToRefresh);
        
        if (!pathsToRegister.isEmpty()) {
            targetAgi.getResourceManager().registerPaths(pathsToRegister, "added to context by user via context menu item");
            fireBatchRefreshRecursive(fosToRefresh);
            log.info("Batch added {} resources to V2 context in session '{}'", 
                    pathsToRegister.size(), targetAgi.getDisplayName());
        }
    }

    /**
     * Recursively collects files that need to be added to the context.
     * 
     * @param fo The current file or folder.
     * @param targetAgi The target session.
     * @param recursive Whether to recurse into subfolders.
     * @param toRegister Output list of paths to register.
     * @param fosToRefresh Output set of file objects to refresh.
     */
    private static void collectAdditions(FileObject fo, Agi targetAgi, boolean recursive, 
                                       List<Path> toRegister, Set<FileObject> fosToRefresh) {
        if (fo.isData()) {
            File file = FileUtil.toFile(fo);
            if (file != null) {
                String path = file.getAbsolutePath();
                if (targetAgi.getResourceManager().findByPath(path).isEmpty()) {
                    toRegister.add(file.toPath());
                    fosToRefresh.add(fo);
                }
            }
        } else if (fo.isFolder()) {
            for (FileObject child : fo.getChildren()) {
                if (child.isData() || recursive) {
                    collectAdditions(child, targetAgi, recursive, toRegister, fosToRefresh);
                }
            }
        }
    }

    /**
     * Removes a file or folder's contents from the specified V2 context.
     * 
     * @param fo The file object to remove.
     * @param targetAgi The target agi session.
     * @param recursive Whether to remove subfolders recursively.
     */
    public static void removeRecursively(FileObject fo, Agi targetAgi, boolean recursive) {
        List<String> idsToRemove = new ArrayList<>();
        Set<FileObject> fosToRefresh = new HashSet<>();
        
        collectRemovals(fo, targetAgi, recursive, idsToRemove, fosToRefresh);
        
        if (!idsToRemove.isEmpty()) {
            for (String id : idsToRemove) {
                targetAgi.getResourceManager().unregister(id);
            }
            fireBatchRefreshRecursive(fosToRefresh);
            log.info("Batch removed {} resources from V2 context in session '{}'", 
                    idsToRemove.size(), targetAgi.getDisplayName());
        }
    }
    
    /**
     * Recursively collects resource IDs that need to be removed from the context.
     * 
     * @param fo The current file or folder.
     * @param targetAgi The target session.
     * @param recursive Whether to recurse into subfolders.
     * @param ids Output list of resource IDs to unregister.
     * @param fos Output set of file objects to refresh.
     */
    private static void collectRemovals(FileObject fo, Agi targetAgi, boolean recursive, 
                                      List<String> ids, Set<FileObject> fos) {
        if (fo.isData()) {
            File file = FileUtil.toFile(fo);
            if (file != null) {
                targetAgi.getResourceManager().findByPath(file.getAbsolutePath()).ifPresent(res -> {
                    ids.add(res.getId());
                    fos.add(fo);
                });
            }
        } else if (fo.isFolder()) {
            for (FileObject child : fo.getChildren()) {
                if (child.isData() || recursive) {
                    collectRemovals(child, targetAgi, recursive, ids, fos);
                }
            }
        }
    }

    /**
     * Checks if a specific file is currently present in a session's V2 context.
     * 
     * @param fo The file to check.
     * @param agi The session to query.
     * @return true if the file is in context.
     */
    public static boolean isInContext(FileObject fo, Agi agi) {
        if (fo.isData()) {
            File file = FileUtil.toFile(fo);
            return file != null && agi.getResourceManager().findByPath(file.getAbsolutePath()).isPresent();
        }
        return false;
    }

    /**
     * Counts how many logically open sessions contain the specified file in 
     * their V2 context.
     * 
     * @param fo The file to check.
     * @return The number of open sessions containing the file.
     */
    public static int countAgisInContext(FileObject fo) {
        int count = 0;
        for (Agi agi : AnahataInstaller.getContainer().getOpenAgis()) {
            if (isInContext(fo, agi)) {
                count++;
            }
        }
        return count;
    }

    /**
     * Returns V2 resource counts for each open session within a folder or package.
     * 
     * @param fo The container to scan.
     * @param recursive Whether to count recursively.
     * @return A map of open sessions to their respective resource counts.
     */
    public static Map<Agi, Integer> getSessionFileCounts(FileObject fo, boolean recursive) {
        // IDENTITY OVER ORDER: Use HashMap to prevent TreeMap corruption when nicknames change
        Map<Agi, Integer> counts = new HashMap<>();
        for (Agi agi : AnahataInstaller.getContainer().getOpenAgis()) {
            int count = countFilesInContext(fo, agi, recursive);
            if (count > 0) {
                counts.put(agi, count);
            }
        }
        return counts;
    }

    /**
     * Internal counter for files belonging to a folder that are in context.
     * 
     * @param fo The folder or file to check.
     * @param agi The session to query.
     * @param recursive Whether to perform a recursive count.
     * @return The number of resources in context.
     */
    private static int countFilesInContext(FileObject fo, Agi agi, boolean recursive) {
        File file = FileUtil.toFile(fo);
        if (file == null) {
            return 0;
        }
        
        // SEMANTIC ALIGNMENT: Convert OS path to URI-style path for cross-platform comparison
        String absolutePath = file.getAbsolutePath().replace("\\", "/");
        if (fo.isData()) {
            return agi.getResourceManager().findByPath(absolutePath).isPresent() ? 1 : 0;
        }

        // Authority: ensure trailing separator for semantic folder matching
        String folderPrefix = absolutePath.endsWith("/") ? absolutePath : absolutePath + "/";
        
        List<uno.anahata.asi.agi.resource.Resource> resources = agi.getResourceManager().getResourcesList();
        return (int) resources.stream()
                .filter(r -> {
                    String path = r.getHandle().getUri().getPath();
                    if (path == null) return false;
                    
                    // Leading Slash Normalization: handle both /home and home
                    String normalizedPath = path.startsWith("/") ? path : "/" + path;
                    String normalizedPrefix = folderPrefix.startsWith("/") ? folderPrefix : "/" + folderPrefix;
                    
                    if (normalizedPath.startsWith(normalizedPrefix)) {
                        if (recursive) return true;
                        String remainder = normalizedPath.substring(normalizedPrefix.length());
                        return !remainder.contains("/");
                    }
                    return false;
                })
                .count();
    }

    /**
     * Fires a batch refresh event for a set of file objects, including their parent hierarchy.
     * 
     * @param targets The set of file objects to refresh.
     */
    private static void fireBatchRefreshRecursive(Set<FileObject> targets) {
        Map<FileSystem, Set<FileObject>> toRefreshByFs = new HashMap<>();
        for (FileObject fo : targets) {
            FileObject current = fo;
            while (current != null) {
                try {
                    FileSystem fs = current.getFileSystem();
                    toRefreshByFs.computeIfAbsent(fs, k -> new HashSet<>()).add(current);
                } catch (Exception ex) {
                    log.warn("Failed to resolve filesystem for refresh: {}", current.getPath(), ex);
                }
                current = current.getParent();
            }
        }
        for (Map.Entry<FileSystem, Set<FileObject>> entry : toRefreshByFs.entrySet()) {
            AnahataAnnotationProvider.fireRefresh(entry.getKey(), entry.getValue());
        }
    }

    /**
     * Fires a recursive refresh event for a single file object and its parents.
     * 
     * @param fo The file object to refresh.
     */
    public static void fireRefreshRecursive(FileObject fo) {
        fireBatchRefreshRecursive(Set.of(fo));
    }
}
