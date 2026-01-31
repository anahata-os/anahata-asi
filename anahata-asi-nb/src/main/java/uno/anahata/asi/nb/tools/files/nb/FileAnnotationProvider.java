/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.files.nb;

import java.awt.Image;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.Action;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.api.project.Project;
import org.netbeans.modules.masterfs.providers.AnnotationProvider;
import org.netbeans.modules.masterfs.providers.InterceptionListener;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileStatusEvent;
import org.openide.filesystems.FileSystem;
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;
import org.openide.util.lookup.ServiceProvider;
import uno.anahata.asi.chat.Chat;

/**
 * Provides file-level annotations (icons and names) in the NetBeans Projects tab.
 * It adds a badge and a chat count label [n] to files and folders that are 
 * currently in one or more active AI contexts.
 * <p>
 * This provider implements a delegation pattern to ensure it doesn't clobber 
 * other annotations (like Git branch names or status badges) in the 
 * MasterFileSystem pipeline.
 * </p>
 * 
 * @author anahata
 */
@ServiceProvider(service = AnnotationProvider.class, position = 10000) 
public class FileAnnotationProvider extends AnnotationProvider {

    private static final Logger LOG = Logger.getLogger(FileAnnotationProvider.class.getName());
    
    private static final Image BADGE;

    static {
        Image original = ImageUtilities.loadImage("icons/anahata.png");
        if (original != null) {
            BADGE = original.getScaledInstance(8, 8, Image.SCALE_SMOOTH);
        } else {
            BADGE = null;
        }
    }

    public FileAnnotationProvider() {
        LOG.info("FileAnnotationProvider instance created.");
    }

    @Override
    public Image annotateIcon(Image icon, int type, Set<? extends FileObject> files) {
        // 1. Delegate to the rest of the chain first to get Git/Error badges
        Image baseIcon = delegateIcon(icon, type, files);
        
        for (FileObject fo : files) {            
            // Skip project roots for badging (handled by AnahataProjectAnnotator)
            if (isProjectRoot(fo)) {
                return baseIcon;
            }
            
            Map<Chat, Integer> sessionCounts = FilesContextActionLogic.getSessionFileCounts(fo);
            if (!sessionCounts.isEmpty() && BADGE != null) {
                // Merge our badge onto the already-annotated baseIcon at 8,0 (Top Right)
                Image badged = ImageUtilities.mergeImages(baseIcon, BADGE, 8, 0);
                
                // Build rich HTML tooltip
                StringBuilder sb = new StringBuilder();
                sb.append("<b>In AI Context:</b><br>");
                for (Map.Entry<Chat, Integer> entry : sessionCounts.entrySet()) {
                    sb.append("&nbsp;&nbsp;&bull;&nbsp;<b>").append(entry.getKey().getDisplayName()).append("</b>");
                    if (fo.isFolder()) {
                        sb.append(": ").append(entry.getValue()).append(" files");
                    }
                    sb.append("<br>");
                }
                
                return mergeTooltip(badged, sb.toString());
            }
        }
        return baseIcon; 
    }

    /**
     * We return null here to avoid clobbering other name annotations (like Git branch names).
     * NetBeans will then fall back to annotateNameHtml or the default name.
     */
    @Override
    public String annotateName(String name, Set<? extends FileObject> files) {
        return null;
    }

    /**
     * This is the preferred way to add status labels. By delegating first, we preserve
     * any previous annotations (like Git's [main]) and then append our label.
     */
    @Override
    public String annotateNameHtml(String name, Set<? extends FileObject> files) {
        // 1. Delegate to get Git branch names etc.
        String delegatedName = delegateNameHtml(name, files);
        String currentName = delegatedName != null ? delegatedName : name;

        for (FileObject fo : files) {
            // Skip project roots (handled by AnahataProjectAnnotator)
            if (isProjectRoot(fo)) {
                return delegatedName;
            }

            Map<Chat, Integer> sessionCounts = FilesContextActionLogic.getSessionFileCounts(fo);
            if (!sessionCounts.isEmpty()) {
                StringBuilder labelBuilder = new StringBuilder();
                labelBuilder.append(" <font color='#707070'>");
                
                if (fo.isData()) {
                    // For files: [SessionName] if 1 session, [n] if multiple
                    if (sessionCounts.size() == 1) {
                        labelBuilder.append("[").append(sessionCounts.keySet().iterator().next().getDisplayName()).append("]");
                    } else {
                        labelBuilder.append("[").append(sessionCounts.size()).append("]");
                    }
                } else {
                    // For folders: [n][m] (one bracket per session with file count)
                    for (Integer count : sessionCounts.values()) {
                        labelBuilder.append("[").append(count).append("]");
                    }
                }
                labelBuilder.append("</font>");
                
                String label = labelBuilder.toString();
                
                // If the name is already HTML, inject our label before the closing tag
                if (currentName.toLowerCase().contains("<html>")) {
                    return currentName.replaceFirst("(?i)</html>", label + "</html>");
                }
                // Otherwise, wrap the whole thing
                return "<html>" + currentName + label + "</html>";
            }
        }
        return delegatedName; 
    }

    /**
     * Manually continues the AnnotationProvider chain for icons.
     */
    private Image delegateIcon(Image icon, int type, Set<? extends FileObject> files) {
        boolean foundSelf = false;
        for (AnnotationProvider ap : Lookup.getDefault().lookupAll(AnnotationProvider.class)) {
            if (!foundSelf) {
                if (ap == this) foundSelf = true;
                continue;
            }
            Image result = ap.annotateIcon(icon, type, files);
            if (result != null) return result;
        }
        return icon;
    }

    /**
     * Manually continues the AnnotationProvider chain for HTML names.
     */
    private String delegateNameHtml(String name, Set<? extends FileObject> files) {
        boolean foundSelf = false;
        for (AnnotationProvider ap : Lookup.getDefault().lookupAll(AnnotationProvider.class)) {
            if (!foundSelf) {
                if (ap == this) foundSelf = true;
                continue;
            }
            String result = ap.annotateNameHtml(name, files);
            if (result != null) return result;
        }
        return null;
    }

    /**
     * Merges a tooltip segment with existing image tooltips.
     */
    private Image mergeTooltip(Image icon, String segment) {
        String existing = ImageUtilities.getImageToolTip(icon);
        if (existing == null || existing.isEmpty()) {
            return ImageUtilities.addToolTipToImage(icon, "<html>" + segment + "</html>");
        }
        
        // If it's already HTML, inject before the closing tag
        if (existing.toLowerCase().contains("<html>")) {
            return ImageUtilities.addToolTipToImage(icon, existing.replaceFirst("(?i)</html>", segment + "</html>"));
        }
        
        // Otherwise, wrap both in HTML
        return ImageUtilities.addToolTipToImage(icon, "<html>" + existing + segment + "</html>");
    }

    private boolean isProjectRoot(FileObject fo) {
        if (fo == null || !fo.isFolder()) return false;
        Project p = FileOwnerQuery.getOwner(fo);
        if (p == null) return false;
        
        FileObject root = p.getProjectDirectory();
        return fo.equals(root) || fo.getPath().equals(root.getPath());
    }

    @Override
    public Action[] actions(Set<? extends FileObject> files) {
        return new Action[0];
    }

    @Override
    public InterceptionListener getInterceptionListener() {
        return null;
    }
    
    public static void fireRefresh(Set<FileObject> files) {
        if (files == null || files.isEmpty()) return;
        try {
            FileSystem fs = files.iterator().next().getFileSystem();
            for (AnnotationProvider ap : Lookup.getDefault().lookupAll(AnnotationProvider.class)) {
                if (ap instanceof FileAnnotationProvider aap) {
                    aap.fireFileStatusChanged(new FileStatusEvent(fs, files, true, true));
                }
            }
        } catch (Exception ex) {
            LOG.log(Level.WARNING, "Failed to fire refresh", ex);
        }
    }
}
