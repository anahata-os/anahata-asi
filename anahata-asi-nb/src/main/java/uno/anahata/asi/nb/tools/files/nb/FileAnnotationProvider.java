/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.files.nb;

import java.awt.Image;
import java.io.File;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.Action;
import javax.swing.SwingUtilities;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.api.project.Project;
import org.netbeans.modules.masterfs.providers.AnnotationProvider;
import org.netbeans.modules.masterfs.providers.InterceptionListener;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileStatusEvent;
import org.openide.filesystems.FileSystem;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataShadow;
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;
import org.openide.util.lookup.ServiceProvider;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.chat.Chat;

/**
 * Master Aggregator for NetBeans file-level annotations. 
 * <p>
 * This provider implements a high-position aggregation pattern (position 10000) 
 * to ensure that all IDE metadata (Git branch names, compilation errors, status colors) 
 * is preserved while overlaying Anahata's context-aware session labels and badges.
 * </p>
 * <p>
 * It handles the "Root Paradox" by using Canonical Paths to resolve project delegates 
 * (like pom.xml) to their folders, ensuring accurate unified totals.
 * </p>
 * 
 * @author anahata
 */
//@ServiceProvider(service = AnnotationProvider.class, position = 2100) 
public class FileAnnotationProvider extends AnnotationProvider {

    private static final Logger LOG = Logger.getLogger(FileAnnotationProvider.class.getName());
    private static final Image BADGE;
    
    /** Prevents infinite loops during manual delegation for HTML names. */
    private static final ThreadLocal<Boolean> DELEGATING = ThreadLocal.withInitial(() -> false);

    static {
        Image original = ImageUtilities.loadImage("icons/anahata_16.png");
        BADGE = (original != null) ? original.getScaledInstance(8, 8, Image.SCALE_SMOOTH) : null;
    }

    /**
     * Default constructor for the master aggregator.
     */
    public FileAnnotationProvider() {
        LOG.info("FileAnnotationProvider (Master Aggregator) Round 9 initializing...");
    }

    /**
     * Annotates file and folder icons.
     * <p>
     * At position 10000, the 'icon' parameter already contains badges and tooltips from 
     * previous providers (like Git or Java Errors). We append our badge at offset 16.
     * </p>
     * 
     * @param icon The icon provided by previous providers.
     * @param type The icon type.
     * @param files Associated files.
     * @return Aggregated icon with Anahata badge.
     */
    @Override
    public Image annotateIcon(Image icon, int type, Set<? extends FileObject> files) {
        Image current = icon;
        for (FileObject fo : files) {            
            if (isProjectRoot(fo)) {
                continue; // AnahataProjectIconAnnotator handles this node
            }

            FileObject res = resolve(fo);
            if (res == null) continue;

            Map<Chat, Integer> sessionCounts = FilesContextActionLogic.getSessionFileCounts(res, res.isFolder());
            if (!sessionCounts.isEmpty() && BADGE != null) {
                current = ImageUtilities.mergeImages(current, BADGE, 16, 0);
                current = mergeTooltip(current, buildTooltip(res, sessionCounts));
            }
        }
        return current; 
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String annotateName(String name, Set<? extends FileObject> files) {
        return null; 
    }

    /**
     * Annotates the HTML display name. 
     * <p>
     * Since NetBeans passes a plain-text 'name' to this method, we manually delegate 
     * to ALL other providers to pick up branch info [main] before appending Anahata labels.
     * </p>
     * 
     * @param name The original plain name.
     * @param files Associated files.
     * @return Final aggregated HTML label.
     */
    @Override
    public String annotateNameHtml(String name, Set<? extends FileObject> files) {
        List<Chat> activeChats = AnahataInstaller.getContainer().getActiveChats();
        
        for (FileObject fo : files) {
            if (isProjectRoot(fo)) {
                return null; // Exclusive Authority: Let AnahataProjectIconAnnotator handle this
            }
        }

        // 1. Delegate first to get Git/Subversion status
        String baseHtml = delegateNameHtml(name, files);
        String currentName = (baseHtml != null) ? baseHtml : (name != null ? name : "");

        for (FileObject fo : files) {
            List<Integer> sessionTotals = calculateSessionTotals(fo, activeChats);
            boolean anyInContext = sessionTotals.stream().anyMatch(i -> i > 0);

            if (anyInContext) {
                String label = buildNameLabel(fo, activeChats, sessionTotals);
                
                if (currentName.toLowerCase().contains("<html>")) {
                    return currentName.replaceFirst("(?i)</html>", label + "</html>");
                }
                return "<html>" + currentName + label + "</html>";
            }
        }
        
        return baseHtml; // Return the delegated result if we have nothing to add
    }

    /**
     * Calculates unified session totals using Canonical Paths to ensure project 
     * providers are correctly identified in the toolkit.
     * 
     * @param fo The file object.
     * @param activeChats List of active sessions.
     * @return List of totals.
     */
    private List<Integer> calculateSessionTotals(FileObject fo, List<Chat> activeChats) {
        List<Integer> totals = new ArrayList<>();
        FileObject res = resolve(fo);
        if (res == null) return totals;
        
        // Round 10: Folders count their local children. Files are 1 or 0.
        Map<Chat, Integer> resourceCounts = FilesContextActionLogic.getSessionFileCounts(res, res.isFolder());
        
        for (Chat chat : activeChats) {
            totals.add(resourceCounts.getOrDefault(chat, 0));
        }
        return totals;
    }

    private String buildNameLabel(FileObject fo, List<Chat> chats, List<Integer> totals) {
        StringBuilder sb = new StringBuilder();
        sb.append(" <font color='#707070'>");
        
        FileObject res = resolve(fo);
        if (res != null && res.isFolder()) {
            for (Integer sum : totals) {
                if (sum > 0) sb.append("[").append(sum).append("]");
            }
        } else {
            long sessionsWithFile = totals.stream().filter(i -> i > 0).count();
            if (sessionsWithFile == 1) {
                int idx = -1;
                for (int i = 0; i < totals.size(); i++) {
                    if (totals.get(i) > 0) { idx = i; break; }
                }
                sb.append("(").append(chats.get(idx).getNickname()).append(")");
            } else if (sessionsWithFile > 1) {
                sb.append("(").append(totals.stream().mapToLong(i -> i).sum()).append(")");
            }
        }
        sb.append("</font>");
        return sb.toString();
    }

    /**
     * Builds the Anahata tooltip segment.
     * 
     * @param fo The file object.
     * @param counts Map of chat to file count.
     * @return Tooltip HTML segment.
     */
    private String buildTooltip(FileObject fo, Map<Chat, Integer> counts) {
        StringBuilder sb = new StringBuilder();
        sb.append("<img src=\"").append(getClass().getResource("/icons/anahata_16.png")).append("\" width=\"12\" height=\"12\"> ");
        sb.append("<b>In Context In:</b><br>");
        List<Chat> sorted = new ArrayList<>(counts.keySet());
        sorted.sort(Comparator.comparing(Chat::getDisplayName));
        for (Chat chat : sorted) {
            sb.append("&nbsp;&nbsp;&bull;&nbsp;").append(chat.getDisplayName())
              .append(": ").append(counts.get(chat)).append(" files<br>");
        }
        return sb.toString();
    }

    /**
     * Manually delegates to all other AnnotationProviders to accumulate status info.
     */
    private String delegateNameHtml(String name, Set<? extends FileObject> files) {
        if (DELEGATING.get()) return null;
        DELEGATING.set(true);
        String current = null;
        try {
            for (AnnotationProvider ap : Lookup.getDefault().lookupAll(AnnotationProvider.class)) {
                if (ap == this) continue;
                String res = ap.annotateNameHtml(current != null ? current : name, files);
                if (res != null) current = res;
            }
        } finally {
            DELEGATING.set(false);
        }
        return current;
    }

    /**
     * Merges Anahata tooltips with existing ones, applying semantic deduplication.
     */
    private Image mergeTooltip(Image icon, String segment) {
        String existing = ImageUtilities.getImageToolTip(icon);
        String clean = deduplicateTooltip(existing);
        if (clean != null) {
            clean = clean.replaceAll("(?i)<img[^>]*anahata_16\\.png[^>]*>.*", "");
        }
        String separator = (clean != null && !clean.isEmpty()) ? "<br><hr>" : "";
        return ImageUtilities.addToolTipToImage(icon, "<html>" + (clean != null ? clean : "") + separator + segment + "</html>");
    }

    /**
     * Normalizes and deduplicates tooltip lines to stop Git stuttering.
     */
    private String deduplicateTooltip(String tooltip) {
        if (tooltip == null) return null;
        String text = tooltip.replaceAll("(?i)</?html>", "");
        String[] lines = text.split("(?i)<br/?>|\\n|<p/?>|<hr/?>");
        Set<String> uniqueLines = new LinkedHashSet<>();
        Set<String> seenPlain = new HashSet<>();
        for (String line : lines) {
            String trimmed = line.trim();
            if (trimmed.isEmpty()) continue;
            // Hyper-semantic comparison: ignore tags and non-alphanumeric
            String plain = trimmed.replaceAll("<[^>]*>", "").replaceAll("[^a-zA-Z0-9\\s]", " ").replaceAll("\\s+", " ").trim().toLowerCase();
            if (!plain.isEmpty() && seenPlain.add(plain)) {
                uniqueLines.add(trimmed);
            }
        }
        return String.join("<br>", uniqueLines);
    }

    /**
     * Resolves shadow files or virtual objects to their physical disk locations.
     */
    private FileObject resolve(FileObject fo) {
        if (fo == null) return null;
        try {
            DataObject dobj = DataObject.find(fo);
            if (dobj instanceof DataShadow ds) {
                return ds.getOriginal().getPrimaryFile();
            }
        } catch (Exception e) {}
        return fo;
    }

    /**
     * Gets the canonical disk path for a FileObject.
     */
    private String getCanonicalPath(FileObject fo) {
        if (fo == null) return null;
        File f = FileUtil.toFile(fo);
        if (f != null) {
            try { return f.getCanonicalPath(); } catch (Exception e) {}
        }
        return fo.getPath();
    }

    /**
     * Universal check for folders or project roots.
     */
    private boolean isRootOrFolder(FileObject fo) {
        FileObject res = resolve(fo);
        return (res != null && res.isFolder()) || isProjectRoot(fo);
    }

    /**
     * Checks if a FileObject represents the logical root of a project using canonical paths.
     */
    private boolean isProjectRoot(FileObject fo) {
        if (fo == null) return false;
        Project p = FileOwnerQuery.getOwner(fo);
        if (p == null) return false;
        
        String prjPath = getCanonicalPath(p.getProjectDirectory());
        String foPath = getCanonicalPath(fo);
        
        return foPath != null && foPath.equals(prjPath);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Action[] actions(Set<? extends FileObject> files) { return new Action[0]; }

    /**
     * {@inheritDoc}
     */
    @Override
    public InterceptionListener getInterceptionListener() { return null; }
    
    /**
     * Fires a refresh event for the status of specific files.
     * 
     * @param fs The filesystem.
     * @param files The files to refresh.
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
