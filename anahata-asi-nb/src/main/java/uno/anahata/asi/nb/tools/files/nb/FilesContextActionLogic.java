/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.files.nb;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
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
import uno.anahata.asi.model.resource.AbstractResource;

/**
 * Stateless utility class containing the core logic for adding and removing 
 * NetBeans FileObjects to/from an AI agi context.
 * <p>
 * It automatically handles both text and binary (multimodal) resources based 
 * on the file's capabilities (cookies) and MIME type. This class is designed 
 * for high-performance batch operations to minimize UI impact.
 * </p>
 * 
 * @author anahata
 */
public class FilesContextActionLogic {

    /** Logger for tracking resource registration and refresh events. */
    private static final Logger LOG = Logger.getLogger(FilesContextActionLogic.class.getName());

    /**
     * Adds a file or folder's contents to the specified agi context recursively.
     * <p>
     * This method is optimized for high performance. It performs a discovery pass 
     * to identify all candidate files, creates the necessary resource instances, 
     * and executes a single batch registration through the ResourceManager. 
     * It also triggers a batch UI refresh across the affected filesystems.
     * </p>
     * 
     * @param fo The starting FileObject (file or folder).
     * @param targetAgi The target agi session.
     * @param recursive True to traverse subfolders.
     */
    public static void addRecursively(FileObject fo, Agi targetAgi, boolean recursive) {
        List<AbstractResource<?, ?>> toRegister = new ArrayList<>();
        Set<FileObject> fosToRefresh = new HashSet<>();
        
        collectAdditions(fo, targetAgi, recursive, toRegister, fosToRefresh);
        
        if (!toRegister.isEmpty()) {
            targetAgi.getResourceManager().registerAll(toRegister);
            fireBatchRefreshRecursive(fosToRefresh);
            LOG.log(Level.INFO, "Batch added {0} resources to session ''{1}''", 
                    new Object[]{toRegister.size(), targetAgi.getDisplayName()});
        }
    }

    /**
     * Discovery helper for collecting files to be added to context.
     * 
     * @param fo The current FileObject.
     * @param targetAgi The target agi session.
     * @param recursive True to recurse into folders.
     * @param toRegister The list to accumulate resources.
     * @param fosToRefresh The set to accumulate files for UI refresh.
     */
    private static void collectAdditions(FileObject fo, Agi targetAgi, boolean recursive, 
                                       List<AbstractResource<?, ?>> toRegister, 
                                       Set<FileObject> fosToRefresh) {
        if (fo.isData()) {
            try {
                File file = FileUtil.toFile(fo);
                if (file != null) {
                    String path = file.getAbsolutePath();
                    if (targetAgi.getResourceManager().findByPath(path).isEmpty()) {
                        DataObject dobj = DataObject.find(fo);
                        AbstractPathResource<?> resource;
                        
                        boolean isTextual = dobj.getLookup().lookup(EditorCookie.class) != null 
                                         || dobj.getLookup().lookup(LineCookie.class) != null;

                        if (isTextual) {
                            resource = new NbTextFileResource(targetAgi.getResourceManager(), fo);
                        } else {
                            resource = new NbBinaryFileResource(targetAgi.getResourceManager(), fo);
                        }
                        
                        String ext = fo.getExt();
                        resource.setIconId("nb.file." + (ext.isEmpty() ? "unknown" : ext));
                        toRegister.add(resource);
                        fosToRefresh.add(fo);
                    }
                }
            } catch (Exception ex) {
                LOG.log(Level.SEVERE, "Error preparing file for context addition: " + fo.getPath(), ex);
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
     * Removes a file or folder's contents from the specified agi context.
     * <p>
     * This method uses the same high-performance batching strategy as addition. 
     * It collects all resource IDs first and performs a single unregistration 
     * event, preventing tree table "event storms" on the UI thread.
     * </p>
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
            targetAgi.getResourceManager().unregisterAll(idsToRemove);
            fireBatchRefreshRecursive(fosToRefresh);
            LOG.log(Level.INFO, "Batch removed {0} resources from session ''{1}''", 
                    new Object[]{idsToRemove.size(), targetAgi.getDisplayName()});
        }
    }
    
    /**
     * Discovery helper for collecting resource IDs to be removed.
     * 
     * @param fo Current FileObject.
     * @param targetAgi Target session.
     * @param recursive True to recurse.
     * @param ids Accumulated resource IDs.
     * @param fos Accumulated FileObjects for UI refresh.
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
     * Fires a batch refresh event for multiple files and all their ancestors.
     * <p>
     * Groups files by their filesystem and identifies all parents up to the 
     * root that require a visual refresh (to update badge counters).
     * </p>
     * 
     * @param targets The set of file objects that changed context status.
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
                    // Ignore invalid filesystems during traversal
                }
                current = current.getParent();
            }
        }
        
        for (Map.Entry<FileSystem, Set<FileObject>> entry : toRefreshByFs.entrySet()) {
            AnahataAnnotationProvider.fireRefresh(entry.getKey(), entry.getValue());
        }
    }

    /**
     * Checks if a specific file is currently present in a session's context.
     * 
     * @param fo The file to check.
     * @param agi The agi session to query.
     * @return True if the file is a managed resource in the session.
     */
    public static boolean isInContext(FileObject fo, Agi agi) {
        if (fo.isData()) {
            File file = FileUtil.toFile(fo);
            return file != null && agi.getResourceManager().findByPath(file.getAbsolutePath()).isPresent();
        }
        return false;
    }

    /**
     * Counts how many active sessions contain the specified file.
     * 
     * @param fo The file object to check.
     * @return The number of sessions containing this file.
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
     * Returns resource counts for each session within a folder or package.
     * 
     * @param fo The folder FileObject.
     * @param recursive True to perform a recursive count of subfolders.
     * @return A sorted map of sessions and their respective resource counts.
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
     * Counts the number of resources in context for a specific session within a folder.
     * <p>
     * Implementation performs a prefix-based path comparison on the session's 
     * active resource list.
     * </p>
     * 
     * @param fo The folder.
     * @param agi The session.
     * @param recursive True for recursive count.
     * @return The resource count.
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
                    }
                    String remainder = path.substring(folderPrefix.length());
                    return !remainder.contains(File.separator);
                })
                .count();
    }

    /**
     * Fires a comprehensive refresh event for a single file and all its ancestors.
     * 
     * @param fo The file object to refresh.
     */
    public static void fireRefreshRecursive(FileObject fo) {
        fireBatchRefreshRecursive(Set.of(fo));
    }
}
