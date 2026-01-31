/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.files.nb;

import java.awt.Image;
import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;
import javax.swing.Icon;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.util.ImageUtilities;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * Stateless utility class containing the core logic for adding and removing 
 * NetBeans FileObjects to/from an AI chat context.
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
                        NbTextFileResource resource = new NbTextFileResource(targetChat.getResourceManager(), fo);
                        
                        DataObject dobj = DataObject.find(fo);
                        if (dobj != null) {
                            String ext = fo.getExt();
                            String iconId = "nb.file." + (ext.isEmpty() ? "unknown" : ext);
                            
                            if (IconUtils.getIcon(iconId) == null) {
                                Image img = dobj.getNodeDelegate().getIcon(java.beans.BeanInfo.ICON_COLOR_16x16);
                                Icon icon = ImageUtilities.image2Icon(img);
                                IconUtils.registerIcon(iconId, icon);
                            }
                            resource.setIconId(iconId);
                        }
                        
                        targetChat.getResourceManager().register(resource);
                        LOG.info("Added file to context of session '" + targetChat.getDisplayName() + "': " + path);
                        fireRefreshRecursive(fo);
                    }
                }
            } catch (Exception ex) {
                LOG.log(java.util.logging.Level.SEVERE, "Error adding file to context", ex);
            }
        } else if (fo.isFolder() && recursive) {
            for (FileObject child : fo.getChildren()) {
                addRecursively(child, targetChat, true);
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
                    LOG.info("Removed file from context of session '" + targetChat.getDisplayName() + "': " + path);
                    fireRefreshRecursive(fo);
                });
            }
        } else if (fo.isFolder() && recursive) {
            for (FileObject child : fo.getChildren()) {
                removeRecursively(child, targetChat, true);
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
     * 
     * @param fo The file or folder to check.
     * @return A map of Chat to file count.
     */
    public static Map<Chat, Integer> getSessionFileCounts(FileObject fo) {
        Map<Chat, Integer> counts = new HashMap<>();
        for (Chat chat : AnahataInstaller.getContainer().getActiveChats()) {
            int count = countFilesInContext(fo, chat);
            if (count > 0) {
                counts.put(chat, count);
            }
        }
        return counts;
    }

    private static int countFilesInContext(FileObject fo, Chat chat) {
        if (fo.isData()) {
            return isInContext(fo, chat) ? 1 : 0;
        }
        int total = 0;
        for (FileObject child : fo.getChildren()) {
            total += countFilesInContext(child, chat);
        }
        return total;
    }

    /**
     * Fires a refresh event for the given file and all its parent folders.
     * This ensures that badges and name annotations propagate up the tree.
     * 
     * @param fo The file object to refresh.
     */
    public static void fireRefreshRecursive(FileObject fo) {
        Set<FileObject> toRefresh = new HashSet<>();
        FileObject current = fo;
        while (current != null) {
            toRefresh.add(current);
            current = current.getParent();
        }
        FileAnnotationProvider.fireRefresh(toRefresh);
    }
}
