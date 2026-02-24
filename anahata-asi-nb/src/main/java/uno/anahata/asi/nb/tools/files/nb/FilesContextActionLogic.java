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
import org.openide.filesystems.FileAttributeEvent;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileSystem;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.model.resource.AbstractPathResource;

/**
 * Stateless utility class containing the core logic for adding and removing 
 * NetBeans FileObjects to/from an AI chat context.
 * <p>
 * It automatically handles both text and binary (multimodal) resources based 
 * on the file's capabilities (cookies) and MIME type.
 * </p>
 * 
 * @author anahata
 */
public class FilesContextActionLogic {

    private static final Logger LOG = Logger.getLogger(FilesContextActionLogic.class.getName());

    /**
     * Adds a file or folder's contents to the specified chat context.
     * 
     * @param fo The file object to add.
     * @param targetChat The target chat session.
     * @param recursive Whether to add subfolders recursively.
     */
    public static void addRecursively(FileObject fo, Chat targetChat, boolean recursive) {
        if (fo.isData()) {
            try {
                File file = FileUtil.toFile(fo);
                if (file != null) {
                    String path = file.getAbsolutePath();
                    if (targetChat.getResourceManager().findByPath(path).isEmpty()) {
                        DataObject dobj = DataObject.find(fo);
                        AbstractPathResource<?> resource;
                        
                        // Proper NetBeans API check: if it has an editor or line cookie, it's textual.
                        boolean isTextual = dobj.getLookup().lookup(EditorCookie.class) != null 
                                         || dobj.getLookup().lookup(LineCookie.class) != null;

                        if (isTextual) {
                            resource = new NbTextFileResource(targetChat.getResourceManager(), fo);
                        } else {
                            resource = new NbBinaryFileResource(targetChat.getResourceManager(), fo);
                            LOG.log(Level.INFO, "Detected binary/multimodal file ({0}): {1}", new Object[]{fo.getMIMEType(), path});
                        }
                        
                        // Set a stable icon ID based on extension. 
                        // The IconProvider will use this as a fallback if live lookup fails.
                        String ext = fo.getExt();
                        resource.setIconId("nb.file." + (ext.isEmpty() ? "unknown" : ext));
                        
                        targetChat.getResourceManager().register(resource);
                        LOG.log(Level.INFO, "Added file to context of session ''{0}'': {1}", new Object[]{targetChat.getDisplayName(), path});
                        fireRefreshRecursive(fo);
                    }
                }
            } catch (Exception ex) {
                LOG.log(Level.SEVERE, "Error adding file to context", ex);
            }
        } else if (fo.isFolder()) {
            for (FileObject child : fo.getChildren()) {
                if (child.isData() || recursive) {
                    addRecursively(child, targetChat, recursive);
                }
            }
        }
    }

    /**
     * Removes a file or folder's contents from the specified chat context.
     * 
     * @param fo The file object to remove.
     * @param targetChat The target chat session.
     * @param recursive Whether to remove subfolders recursively.
     */
    public static void removeRecursively(FileObject fo, Chat targetChat, boolean recursive) {
        if (fo.isData()) {
            File file = FileUtil.toFile(fo);
            if (file != null) {
                String path = file.getAbsolutePath();
                targetChat.getResourceManager().findByPath(path).ifPresent(res -> {
                    targetChat.getResourceManager().unregister(res.getId());
                    LOG.log(Level.INFO, "Removed file from context of session ''{0}'': {1}", new Object[]{targetChat.getDisplayName(), path});
                    fireRefreshRecursive(fo);
                });
            }
        } else if (fo.isFolder()) {
            for (FileObject child : fo.getChildren()) {
                if (child.isData() || recursive) {
                    removeRecursively(child, targetChat, recursive);
                }
            }
        }
    }

    /**
     * Checks if the given file is currently in the target chat's context.
     * 
     * @param fo The file object to check.
     * @param chat The chat session to check against.
     * @return {@code true} if the file is in the context.
     */
    public static boolean isInContext(FileObject fo, Chat chat) {
        if (fo.isData()) {
            File file = FileUtil.toFile(fo);
            return file != null && chat.getResourceManager().findByPath(file.getAbsolutePath()).isPresent();
        }
        return false;
    }

    /**
     * Counts how many active chat sessions have the given file in their context.
     * 
     * @param fo The file object to check.
     * @return The number of chats containing the file.
     */
    public static int countChatsInContext(FileObject fo) {
        int count = 0;
        for (Chat chat : AnahataInstaller.getContainer().getActiveChats()) {
            if (isInContext(fo, chat)) {
                count++;
            }
        }
        return count;
    }

    /**
     * Returns a map of chat sessions and the number of files they have in context 
     * within the given folder (or 1 if it's a single file).
     * The map is sorted by chat display name for consistent UI rendering.
     * 
     * @param fo The file or folder to check.
     * @param recursive Whether to count files in subfolders.
     * @return A sorted map of Chat to file count.
     */
    public static Map<Chat, Integer> getSessionFileCounts(FileObject fo, boolean recursive) {
        Map<Chat, Integer> counts = new TreeMap<>((c1, c2) -> c1.getDisplayName().compareTo(c2.getDisplayName()));
        for (Chat chat : AnahataInstaller.getContainer().getActiveChats()) {
            int count = countFilesInContext(fo, chat, recursive);
            if (count > 0) {
                counts.put(chat, count);
            }
        }
        return counts;
    }

    /**
     * Counts the number of files in context for a specific chat within a folder.
     * 
     * @param fo The file or folder to check.
     * @param chat The chat session.
     * @param recursive Whether to count files in subfolders.
     * @return The file count.
     */
    private static int countFilesInContext(FileObject fo, Chat chat, boolean recursive) {
        File file = FileUtil.toFile(fo);
        if (file == null) {
            return 0;
        }
        String absolutePath = file.getAbsolutePath();

        if (fo.isData()) {
            return chat.getResourceManager().findByPath(absolutePath).isPresent() ? 1 : 0;
        }

        // Folder logic: path-based counting using registered resources
        String folderPrefix = absolutePath.endsWith(File.separator) ? absolutePath : absolutePath + File.separator;

        return (int) chat.getResourceManager().getResources().stream()
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
     * Fires a refresh event for the given file and all its parent folders.
     * This ensures that badges and name annotations propagate up the tree.
     * 
     * @param fo The file object to refresh.
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
