/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.files.nb;

import java.awt.Image;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.swing.Action;
import javax.swing.SwingUtilities;
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
import uno.anahata.asi.AnahataInstaller;
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
    
    /** The badge image to overlay on file icons. */
    private static final Image BADGE;

    static {
        Image original = ImageUtilities.loadImage("icons/anahata_16.png");
        if (original != null) {
            BADGE = original.getScaledInstance(8, 8, Image.SCALE_SMOOTH);
        } else {
            BADGE = null;
        }
    }

    /**
     * Default constructor for the provider.
     */
    public FileAnnotationProvider() {
        LOG.info("FileAnnotationProvider instance created.");
    }

    /**
     * {@inheritDoc}
     * Annotates the icon with an Anahata badge. 
     * Uses manual aggregation to preserve Git/Error badges.
     */
    @Override
    public Image annotateIcon(Image icon, int type, Set<? extends FileObject> files) {
        // 1. Manually aggregate from other providers first
        Image baseIcon = delegateIcon(icon, type, files);
        
        for (FileObject fo : files) {            
            // Skip project roots for badging (handled by AnahataProjectIconAnnotator)
            if (isProjectRoot(fo)) {
                return baseIcon;
            }
            
            boolean recursive = !isLogicalView();
            Map<Chat, Integer> sessionCounts = FilesContextActionLogic.getSessionFileCounts(fo, recursive);
            
            if (!sessionCounts.isEmpty() && BADGE != null) {
                Image badged = ImageUtilities.mergeImages(baseIcon, BADGE, 16, 0);
                
                StringBuilder sb = new StringBuilder();
                sb.append("<img src=\"").append(getClass().getResource("/icons/anahata_16.png")).append("\" width=\"12\" height=\"12\"> ");
                sb.append("<b>In Context In:</b><br>");
                
                List<Chat> sortedSessions = new ArrayList<>(sessionCounts.keySet());
                sortedSessions.sort(Comparator.comparing(Chat::getDisplayName));
                
                if (fo.isData()) {
                    sb.append("&nbsp;&nbsp;&bull;&nbsp;");
                    sb.append(sortedSessions.stream().map(Chat::getDisplayName).collect(Collectors.joining(", ")));
                } else {
                    for (Chat chat : sortedSessions) {
                        sb.append("&nbsp;&nbsp;&bull;&nbsp;").append(chat.getDisplayName())
                          .append(": ").append(sessionCounts.get(chat)).append(" files<br>");
                    }
                }
                
                return mergeTooltip(badged, sb.toString());
            }
        }
        return baseIcon; 
    }

    /**
     * {@inheritDoc}
     * We return null here to avoid clobbering other name annotations (like Git branch names).
     * NetBeans will then fall back to annotateNameHtml or the default name.
     */
    @Override
    public String annotateName(String name, Set<? extends FileObject> files) {
        return null;
    }

    /**
     * {@inheritDoc}
     * Merges session nicknames/counts into the HTML name, preserving Git branch info.
     */
    @Override
    public String annotateNameHtml(String name, Set<? extends FileObject> files) {
        // 1. Delegate to get existing HTML (Git branch, colors, etc.)
        String delegatedName = delegateNameHtml(name, files);
        String currentName = delegatedName != null ? delegatedName : name;

        for (FileObject fo : files) {
            boolean recursive = !isLogicalView();
            Map<Chat, Integer> sessionCounts = FilesContextActionLogic.getSessionFileCounts(fo, recursive);
            
            if (!sessionCounts.isEmpty()) {
                StringBuilder labelBuilder = new StringBuilder();
                labelBuilder.append(" <font color='#707070'>");
                
                if (fo.isData()) {
                    if (sessionCounts.size() == 1) {
                        labelBuilder.append("[").append(sessionCounts.keySet().iterator().next().getNickname()).append("]");
                    } else {
                        labelBuilder.append("(").append(sessionCounts.size()).append(")");
                    }
                } else {
                    Map<String, Integer> sortedCounts = new TreeMap<>();
                    for (Map.Entry<Chat, Integer> entry : sessionCounts.entrySet()) {
                        sortedCounts.put(entry.getKey().getDisplayName(), entry.getValue());
                    }
                    for (Integer count : sortedCounts.values()) {
                        labelBuilder.append("[").append(count).append("]");
                    }
                }
                labelBuilder.append("</font>");
                
                String label = labelBuilder.toString();
                
                if (currentName.toLowerCase().contains("<html>")) {
                    return currentName.replaceFirst("(?i)</html>", label + "</html>");
                }
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
     * Merges our custom HTML tooltip segment with any existing tooltip on the image.
     * It performs deduplication to fix the Git double-tooltip issue and ensures 
     * proper separation with a horizontal rule.
     * 
     * @param icon The image to annotate.
     * @param segment The HTML segment to append.
     * @return The image with the combined tooltip.
     */
    private Image mergeTooltip(Image icon, String segment) {
        String existing = ImageUtilities.getImageToolTip(icon);
        if (existing == null || existing.isEmpty()) {
            return ImageUtilities.addToolTipToImage(icon, "<html>" + segment + "</html>");
        }
        
        // Deduplicate existing lines to fix the Git double-tooltip issue
        String cleanExisting = deduplicateTooltip(existing);
        
        // Ensure our segment starts on a new line/separator
        String separator = "<br><hr>";
        
        return ImageUtilities.addToolTipToImage(icon, "<html>" + cleanExisting + separator + segment + "</html>");
    }

    /**
     * Strips HTML tags and deduplicates lines in a tooltip string.
     * 
     * @param tooltip The raw tooltip string.
     * @return A cleaned, deduplicated string.
     */
    private String deduplicateTooltip(String tooltip) {
        if (tooltip == null) return null;
        String text = tooltip.replaceAll("(?i)</?html>", "");
        String[] lines = text.split("(?i)<br/?>|\n|<p/?>");
        Set<String> seenPlain = new HashSet<>();
        List<String> resultLines = new ArrayList<>();
        for (String line : lines) {
            String plain = line.replaceAll("<[^>]*>", "").trim();
            if (!plain.isEmpty() && seenPlain.add(plain)) {
                resultLines.add(line.trim());
            }
        }
        return String.join("<br>", resultLines);
    }

    /**
     * Checks if the current call is coming from the Logical View (Projects tab).
     * 
     * @return {@code true} if in Logical View.
     */
    private boolean isLogicalView() {
        for (StackTraceElement element : Thread.currentThread().getStackTrace()) {
            String className = element.getClassName();
            if (className.contains("LogicalView") ||
                className.contains("PackageView") ||
                className.contains("ProjectsRootNode")) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if the given FileObject is a project root.
     */
    private boolean isProjectRoot(FileObject fo) {
        if (fo == null || !fo.isFolder()) return false;
        Project p = FileOwnerQuery.getOwner(fo);
        if (p == null) return false;
        
        FileObject root = p.getProjectDirectory();
        return fo.equals(root) || fo.getPath().equals(root.getPath());
    }

    /** {@inheritDoc} */
    @Override
    public Action[] actions(Set<? extends FileObject> files) {
        return new Action[0];
    }

    /** {@inheritDoc} */
    @Override
    public InterceptionListener getInterceptionListener() {
        return null;
    }
    
    /**
     * Fires a refresh event for the given files on the specified filesystem.
     * This method ensures the refresh is triggered on the Event Dispatch Thread (EDT).
     * 
     * @param fs The filesystem containing the files.
     * @param files The set of files to refresh.
     */
    public static void fireRefresh(FileSystem fs, Set<FileObject> files) {
        if (files == null || files.isEmpty() || fs == null) return;
        SwingUtilities.invokeLater(() -> {
            try {
                for (AnnotationProvider ap : Lookup.getDefault().lookupAll(AnnotationProvider.class)) {
                    if (ap instanceof FileAnnotationProvider aap) {
                        aap.fireFileStatusChanged(new FileStatusEvent(fs, files, true, true));
                    }
                }
            } catch (Exception ex) {
                LOG.log(Level.WARNING, "Failed to fire refresh", ex);
            }
        });
    }
}
