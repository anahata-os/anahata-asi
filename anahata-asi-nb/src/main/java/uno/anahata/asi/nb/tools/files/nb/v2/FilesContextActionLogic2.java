/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.files.nb.v2;

import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import lombok.extern.slf4j.Slf4j;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileSystem;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.nb.tools.files.nb.AnahataAnnotationProvider;
import uno.anahata.asi.resource.v2.Resource;
import uno.anahata.asi.resource.v2.Resources;

/**
 * Logic handler for adding NetBeans files and folders to the V2 AI context.
 * <p>
 * This class bridges the IDE's {@link FileObject} selections with the 
 * {@link uno.anahata.asi.resource.v2.ResourceManager2}. It handles recursion 
 * for folders and provides a V1-compatible API for seamless migration.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class FilesContextActionLogic2 {

    private static final Logger LOG = Logger.getLogger(FilesContextActionLogic2.class.getName());

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
            targetAgi.getResourceManager2().registerPaths(pathsToRegister, "added to context by user via context menu item");
            fireBatchRefreshRecursive(fosToRefresh);
            log.info("Batch added {} resources to V2 context in session '{}'", 
                    pathsToRegister.size(), targetAgi.getDisplayName());
        }
    }

    private static void collectAdditions(FileObject fo, Agi targetAgi, boolean recursive, 
                                       List<Path> toRegister, Set<FileObject> fosToRefresh) {
        if (fo.isData()) {
            File file = FileUtil.toFile(fo);
            if (file != null) {
                String path = file.getAbsolutePath();
                if (targetAgi.getResourceManager2().findByPath(path).isEmpty()) {
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
                targetAgi.getResourceManager2().unregister(id);
            }
            fireBatchRefreshRecursive(fosToRefresh);
            log.info("Batch removed {} resources from V2 context in session '{}'", 
                    idsToRemove.size(), targetAgi.getDisplayName());
        }
    }
    
    private static void collectRemovals(FileObject fo, Agi targetAgi, boolean recursive, 
                                      List<String> ids, Set<FileObject> fos) {
        if (fo.isData()) {
            File file = FileUtil.toFile(fo);
            if (file != null) {
                targetAgi.getResourceManager2().findByPath(file.getAbsolutePath()).ifPresent(res -> {
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
     */
    public static boolean isInContext(FileObject fo, Agi agi) {
        if (fo.isData()) {
            File file = FileUtil.toFile(fo);
            return file != null && agi.getResourceManager2().findByPath(file.getAbsolutePath()).isPresent();
        }
        return false;
    }

    /**
     * Counts how many active sessions contain the specified file in their V2 context.
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
     * Returns V2 resource counts for each session within a folder or package.
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

    private static int countFilesInContext(FileObject fo, Agi agi, boolean recursive) {
        File file = FileUtil.toFile(fo);
        if (file == null) return 0;
        String absolutePath = file.getAbsolutePath();

        if (fo.isData()) {
            return agi.getResourceManager2().findByPath(absolutePath).isPresent() ? 1 : 0;
        }

        String folderPrefix = absolutePath.endsWith(File.separator) ? absolutePath : absolutePath + File.separator;

        return (int) agi.getResourceManager2().getResourcesList().stream()
                .filter(r -> {
                    String path = r.getHandle().getUri().getPath();
                    if (path == null || !path.startsWith(folderPrefix)) return false;
                    if (recursive) return true;
                    String remainder = path.substring(folderPrefix.length());
                    return !remainder.contains(File.separator);
                })
                .count();
    }

    private static void fireBatchRefreshRecursive(Set<FileObject> targets) {
        Map<FileSystem, Set<FileObject>> toRefreshByFs = new HashMap<>();
        for (FileObject fo : targets) {
            FileObject current = fo;
            while (current != null) {
                try {
                    FileSystem fs = current.getFileSystem();
                    toRefreshByFs.computeIfAbsent(fs, k -> new HashSet<>()).add(current);
                } catch (Exception ex) {}
                current = current.getParent();
            }
        }
        for (Map.Entry<FileSystem, Set<FileObject>> entry : toRefreshByFs.entrySet()) {
            AnahataAnnotationProvider.fireRefresh(entry.getKey(), entry.getValue());
        }
    }

    public static void fireRefreshRecursive(FileObject fo) {
        fireBatchRefreshRecursive(Set.of(fo));
    }
}
