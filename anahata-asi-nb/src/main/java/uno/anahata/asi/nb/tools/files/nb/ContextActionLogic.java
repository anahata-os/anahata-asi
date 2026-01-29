/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.files.nb;

import java.awt.Image;
import java.io.File;
import java.util.Collections;
import java.util.logging.Logger;
import javax.swing.Icon;
import org.netbeans.api.project.Project;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.util.ImageUtilities;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.model.resource.TextFileResource;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * Stateless utility class containing the core logic for adding and removing 
 * NetBeans FileObjects and Projects to/from an AI chat context.
 * <p>
 * This class handles recursive folder traversal, authentic icon registration,
 * and resource management, ensuring that the logic is decoupled from the 
 * Swing Action and Lookup listener machinery.
 * </p>
 * 
 * @author anahata
 */
public class ContextActionLogic {

    private static final Logger LOG = Logger.getLogger(ContextActionLogic.class.getName());

    /**
     * Recursively adds a file or folder's contents to the specified chat context.
     * 
     * @param fo The file object to add.
     * @param targetChat The target chat session.
     */
    public static void addRecursively(FileObject fo, Chat targetChat) {
        if (fo.isData()) {
            try {
                File file = FileUtil.toFile(fo);
                if (file != null) {
                    String path = file.getAbsolutePath();
                    // Check if already loaded in this chat
                    if (targetChat.getResourceManager().findByPath(path).isEmpty()) {
                        TextFileResource resource = new TextFileResource(targetChat.getResourceManager(), file.toPath());
                        
                        // Fetch and register the authentic NetBeans file icon
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
                        LOG.info("Added file to context of chat '" + targetChat.getNickname() + "': " + path);
                        
                        // Trigger UI refresh for the file decoration
                        FileAnnotationProvider.fireRefresh(Collections.singleton(fo));
                    }
                }
            } catch (Exception ex) {
                LOG.log(java.util.logging.Level.SEVERE, "Error adding file to context", ex);
            }
        } else if (fo.isFolder()) {
            for (FileObject child : fo.getChildren()) {
                addRecursively(child, targetChat);
            }
        }
    }

    /**
     * Recursively removes a file or folder's contents from the specified chat context.
     * 
     * @param fo The file object to remove.
     * @param targetChat The target chat session.
     */
    public static void removeRecursively(FileObject fo, Chat targetChat) {
        if (fo.isData()) {
            File file = FileUtil.toFile(fo);
            if (file != null) {
                String path = file.getAbsolutePath();
                targetChat.getResourceManager().findByPath(path).ifPresent(res -> {
                    targetChat.getResourceManager().unregister(res.getId());
                    LOG.info("Removed file from context of chat '" + targetChat.getNickname() + "': " + path);
                    // Trigger UI refresh for the file decoration
                    FileAnnotationProvider.fireRefresh(Collections.singleton(fo));
                });
            }
        } else if (fo.isFolder()) {
            for (FileObject child : fo.getChildren()) {
                removeRecursively(child, targetChat);
            }
        }
    }

    /**
     * Checks if the given file or any of its children are missing from the target chat's context.
     * 
     * @param fo The file object to check.
     * @param targetChat The chat session to check against.
     * @return {@code true} if at least one file is missing from the context.
     */
    public static boolean isAnyMissing(FileObject fo, Chat targetChat) {
        if (fo.isData()) {
            File file = FileUtil.toFile(fo);
            return file != null && targetChat.getResourceManager().findByPath(file.getAbsolutePath()).isEmpty();
        } else if (fo.isFolder()) {
            for (FileObject child : fo.getChildren()) {
                if (isAnyMissing(child, targetChat)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Checks if the given file or any of its children are currently in the target chat's context.
     * 
     * @param fo The file object to check.
     * @param targetChat The chat session to check against.
     * @return {@code true} if at least one file is in the context.
     */
    public static boolean isAnyInContext(FileObject fo, Chat targetChat) {
        if (fo.isData()) {
            File file = FileUtil.toFile(fo);
            return file != null && targetChat.getResourceManager().findByPath(file.getAbsolutePath()).isPresent();
        } else if (fo.isFolder()) {
            for (FileObject child : fo.getChildren()) {
                if (isAnyInContext(child, targetChat)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Checks if a specific project is currently active in the given chat's context.
     * 
     * @param project The project to check.
     * @param chat The chat session to check against.
     * @return {@code true} if the project is in context.
     */
    public static boolean isProjectInContext(Project project, Chat chat) {
        return chat.getToolManager().getToolkitInstance(Projects.class)
                .flatMap(pt -> pt.getProjectProvider(project.getProjectDirectory().getPath()))
                .map(pcp -> pcp.isProviding())
                .orElse(false);
    }
}
