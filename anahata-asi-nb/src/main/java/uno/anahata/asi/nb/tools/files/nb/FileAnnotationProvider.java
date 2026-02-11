/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.files.nb;

import java.awt.Image;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
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
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;
import org.openide.util.lookup.ServiceProvider;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.chat.Chat;

/**
 * Master Aggregator for file-level annotations. 
 * It delegates to all other providers to preserve Git/IDE status, then overlays Anahata metadata.
 * Uses a ThreadLocal guard to prevent recursive delegation loops.
 * 
 * @author anahata
 */
@ServiceProvider(service = AnnotationProvider.class, position = 10000) 
public class FileAnnotationProvider extends AnnotationProvider {

    private static final Logger LOG = Logger.getLogger(FileAnnotationProvider.class.getName());
    private static final Image BADGE;
    
    /** Prevents infinite loops during manual delegation to other providers. */
    private static final ThreadLocal<Boolean> DELEGATING = ThreadLocal.withInitial(() -> false);

    static {
        Image original = ImageUtilities.loadImage("icons/anahata_16.png");
        BADGE = (original != null) ? original.getScaledInstance(8, 8, Image.SCALE_SMOOTH) : null;
    }

    /**
     * Default constructor.
     */
    public FileAnnotationProvider() {
        LOG.info("FileAnnotationProvider (Master Aggregator) instance created.");
    }

    @Override
    public Image annotateIcon(Image icon, int type, Set<? extends FileObject> files) {
        // 1. Get base icon from other providers (Git, etc.)
        Image baseIcon = delegateIcon(icon, type, files);
        
        for (FileObject fo : files) {            
            // Exclusive Icon Badging: Skip project roots in Logical View (handled by ProjectIconAnnotator)
            if (isLogicalView() && isProjectRoot(fo)) {
                return baseIcon;
            }
            
            // Physical badging for files and folders (including roots in Files tab)
            Map<Chat, Integer> sessionCounts = FilesContextActionLogic.getSessionFileCounts(fo, !isLogicalView());
            if (!sessionCounts.isEmpty() && BADGE != null) {
                Image badged = ImageUtilities.mergeImages(baseIcon, BADGE, 16, 0);
                return mergeTooltip(badged, buildTooltip(fo, sessionCounts));
            }
        }
        return baseIcon; 
    }

    @Override
    public String annotateName(String name, Set<? extends FileObject> files) {
        return delegateName(name, files);
    }

    @Override
    public String annotateNameHtml(String name, Set<? extends FileObject> files) {
        // 1. Delegate to pick up Git branch info, status colors, etc.
        String baseName = delegateNameHtml(name, files);
        String currentName = (baseName != null) ? baseName : name;

        List<Chat> activeChats = AnahataInstaller.getContainer().getActiveChats();

        for (FileObject fo : files) {
            // Unified Totals: Files, Folders, and Project Roots are all annotated here.
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
        
        // Return delegated name if not in context to allow cleanup/preservation
        return baseName; 
    }

    private List<Integer> calculateSessionTotals(FileObject fo, List<Chat> activeChats) {
        List<Integer> totals = new ArrayList<>();
        Map<Chat, Integer> resourceCounts = FilesContextActionLogic.getSessionFileCounts(fo, !isLogicalView());
        
        for (Chat chat : activeChats) {
            int total = resourceCounts.getOrDefault(chat, 0);
            
            // For project roots, we add the provider counts
            if (isProjectRoot(fo)) {
                final String path = fo.getPath();
                total += chat.getToolManager().getToolkitInstance(uno.anahata.asi.nb.tools.project.Projects.class)
                    .flatMap(t -> t.getProjectProvider(path))
                    .map(pcp -> pcp.isProviding() ? 1 + (int)pcp.getChildrenProviders().stream()
                            .filter(uno.anahata.asi.context.ContextProvider::isProviding).count() : 0)
                    .orElse(0);
            }
            totals.add(total);
        }
        return totals;
    }

    private String buildNameLabel(FileObject fo, List<Chat> chats, List<Integer> totals) {
        StringBuilder sb = new StringBuilder();
        sb.append(" <font color='#707070'>");
        
        if (fo.isData() && !isProjectRoot(fo)) {
            // Files: (displayName) or (total_count)
            long sessionsWithFile = totals.stream().filter(i -> i > 0).count();
            if (sessionsWithFile == 1) {
                int idx = -1;
                for (int i = 0; i < totals.size(); i++) {
                    if (totals.get(i) > 0) { idx = i; break; }
                }
                sb.append("(").append(chats.get(idx).getDisplayName()).append(")");
            } else {
                sb.append("(").append(totals.stream().mapToLong(i -> i).sum()).append(")");
            }
        } else {
            // Folders/Projects: [sum1][sum2]... based on session index
            for (Integer sum : totals) {
                sb.append("[").append(sum).append("]");
            }
        }
        sb.append("</font>");
        return sb.toString();
    }

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

    private Image delegateIcon(Image icon, int type, Set<? extends FileObject> files) {
        if (DELEGATING.get()) return icon;
        DELEGATING.set(true);
        try {
            for (AnnotationProvider ap : Lookup.getDefault().lookupAll(AnnotationProvider.class)) {
                if (ap == this) continue;
                Image res = ap.annotateIcon(icon, type, files);
                if (res != null) return res;
            }
        } finally {
            DELEGATING.set(false);
        }
        return icon;
    }

    private String delegateName(String name, Set<? extends FileObject> files) {
        if (DELEGATING.get()) return name;
        DELEGATING.set(true);
        try {
            for (AnnotationProvider ap : Lookup.getDefault().lookupAll(AnnotationProvider.class)) {
                if (ap == this) continue;
                String res = ap.annotateName(name, files);
                if (res != null) return res;
            }
        } finally {
            DELEGATING.set(false);
        }
        return name;
    }

    private String delegateNameHtml(String name, Set<? extends FileObject> files) {
        if (DELEGATING.get()) return null;
        DELEGATING.set(true);
        try {
            for (AnnotationProvider ap : Lookup.getDefault().lookupAll(AnnotationProvider.class)) {
                if (ap == this) continue;
                String res = ap.annotateNameHtml(name, files);
                if (res != null) return res;
            }
        } finally {
            DELEGATING.set(false);
        }
        return null;
    }

    private Image mergeTooltip(Image icon, String segment) {
        String existing = ImageUtilities.getImageToolTip(icon);
        String clean = deduplicateTooltip(existing);
        
        if (clean != null) {
            // Remove our own previous segment if it exists
            clean = clean.replaceAll("(?i)<img[^>]*anahata_16\\.png[^>]*>.*", "");
        }
        
        String separator = (clean != null && !clean.isEmpty()) ? "<br><hr>" : "";
        return ImageUtilities.addToolTipToImage(icon, "<html>" + (clean != null ? clean : "") + separator + segment + "</html>");
    }

    private String deduplicateTooltip(String tooltip) {
        if (tooltip == null) return null;
        String text = tooltip.replaceAll("(?i)</?html>", "");
        String[] lines = text.split("(?i)<br/?>|\\n|<p/?>|<hr/?>");
        
        java.util.Set<String> uniqueLines = new java.util.LinkedHashSet<>();
        java.util.Set<String> seenPlain = new java.util.HashSet<>();
        
        for (String line : lines) {
            String trimmed = line.trim();
            if (trimmed.isEmpty()) continue;
            String plain = trimmed.replaceAll("<[^>]*>", "").replaceAll("\\s+", " ").trim();
            if (!plain.isEmpty() && seenPlain.add(plain)) {
                uniqueLines.add(trimmed);
            }
        }
        return String.join("<br>", uniqueLines);
    }

    private boolean isLogicalView() {
        return StackWalker.getInstance().walk(s -> s.anyMatch(f -> f.getClassName().contains("org.netbeans.modules.project.ui")));
    }

    private boolean isProjectRoot(FileObject fo) {
        if (fo == null || !fo.isFolder()) return false;
        Project p = FileOwnerQuery.getOwner(fo);
        if (p == null) return false;
        return fo.equals(p.getProjectDirectory());
    }

    @Override
    public Action[] actions(Set<? extends FileObject> files) {
        return new Action[0];
    }

    @Override
    public InterceptionListener getInterceptionListener() {
        return null;
    }
    
    /**
     * Fires a refresh event for the given files on the specified filesystem.
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
