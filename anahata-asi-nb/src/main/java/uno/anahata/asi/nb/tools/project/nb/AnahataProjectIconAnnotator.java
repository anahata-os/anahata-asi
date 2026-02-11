/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.nb;

import java.awt.Image;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ProjectIconAnnotator;
import org.openide.filesystems.FileObject;
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;
import org.openide.util.lookup.ServiceProvider;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.nb.tools.files.nb.FileAnnotationProvider;

/**
 * Annotates project icons in the NetBeans Projects tab with an Anahata badge 
 * if the project is currently active in any AI chat context.
 * <p>
 * This class implements {@link ProjectIconAnnotator} to provide visual feedback 
 * in the IDE's project explorer. It delegates context status checks to 
 * {@link ProjectsContextActionLogic}.
 * </p>
 * 
 * @author anahata
 */
@ServiceProvider(service = ProjectIconAnnotator.class)
public class AnahataProjectIconAnnotator implements ProjectIconAnnotator, ChangeListener {
    private static final Logger LOG = Logger.getLogger(AnahataProjectIconAnnotator.class.getName());
    
    /** 
     * The badge image to overlay on project icons. 
     * FIXED: Renamed to avoid shadowing V1's icon.
     */
    private static final String BADGE_ICON_PATH = "icons/v2/anahata.png";
    private static final Image BADGE;
    
    /** The list of change listeners for icon refresh. */
    private final javax.swing.event.EventListenerList listeners = new javax.swing.event.EventListenerList();

    static {
        LOG.info("AnahataProjectIconAnnotator class loaded.");
        Image img = ImageUtilities.loadImage(BADGE_ICON_PATH);
        if (img != null) {
            BADGE = img.getScaledInstance(8, 8, Image.SCALE_SMOOTH);
        } else {
            BADGE = null;
        }
    }

    /**
     * Default constructor for the annotator.
     */
    public AnahataProjectIconAnnotator() {
        LOG.log(Level.INFO, "AnahataProjectIconAnnotator instance created.");
    }
    
    /**
     * {@inheritDoc}
     * Annotates the project icon with a badge if the project is in an active AI context.
     * It also builds a rich HTML tooltip listing active sessions and their providers.
     */
    @Override
    public Image annotateIcon(Project p, Image icon, boolean opened) {
        String existingTooltip = ImageUtilities.getImageToolTip(icon);
        String projectPath = p.getProjectDirectory().getPath();
        List<Chat> activeChats = AnahataInstaller.getContainer().getActiveChats();
        
        List<String> tooltipLines = new ArrayList<>();

        for (Chat chat : activeChats) {
            final int[] total = {0};
            chat.getToolManager().getToolkitInstance(Projects.class).ifPresent(projectsTool -> {
                projectsTool.getProjectProvider(projectPath).ifPresent(pcp -> {
                    List<String> activeChildren = new ArrayList<>();
                    if (pcp.isProviding()) {
                        total[0]++; // Overview
                        List<String> children = pcp.getChildrenProviders().stream()
                                .filter(ContextProvider::isProviding)
                                .map(ContextProvider::getName)
                                .collect(Collectors.toList());
                        activeChildren.addAll(children);
                        total[0] += children.size();
                    }
                    
                    // Add files (recursive count for unified total)
                    int fileCount = uno.anahata.asi.nb.tools.files.nb.FilesContextActionLogic.getSessionFileCounts(p.getProjectDirectory(), true).getOrDefault(chat, 0);
                    total[0] += fileCount;
                    
                    if (total[0] > 0) {
                        String line = "<b>" + chat.getDisplayName() + "</b> [" + total[0] + "]";
                        if (!activeChildren.isEmpty()) {
                            line += ": " + String.join(", ", activeChildren);
                        }
                        if (fileCount > 0) {
                            line += " (" + fileCount + " files)";
                        }
                        tooltipLines.add(line);
                    }
                });
            });
        }

        // Build the Anahata-specific tooltip segment
        StringBuilder sb = new StringBuilder();
        sb.append("<img src=\"").append(getClass().getResource("/icons/anahata_16.png")).append("\" width=\"12\" height=\"12\"> ");
        
        if (!tooltipLines.isEmpty()) {
            sb.append("<b>In Context in:</b><br>");
            for (String line : tooltipLines) {
                sb.append("&nbsp;&nbsp;&bull;&nbsp;").append(line).append("<br>");
            }
            
            if (BADGE != null) {
                // Offset 8 for projects as requested.
                Image badgedIcon = ImageUtilities.mergeImages(icon, BADGE, 8, 0);
                return mergeTooltip(badgedIcon, sb.toString(), existingTooltip);
            }
        } else {
            sb.append("Not in context in any session");
            return mergeTooltip(icon, sb.toString(), existingTooltip);
        }

        return icon;
    }

    /**
     * Merges our custom HTML tooltip segment with any existing tooltip on the image.
     * It performs deduplication to fix the Git double-tooltip issue and ensures 
     * proper separation with a horizontal rule.
     * 
     * @param icon The image to annotate.
     * @param segment The HTML segment to append.
     * @param existing An optional pre-captured existing tooltip to merge with.
     * @return The image with the combined tooltip.
     */
    private Image mergeTooltip(Image icon, String segment, String existing) {
        String base = (existing != null && !existing.isEmpty()) ? existing : ImageUtilities.getImageToolTip(icon);
        if (base == null || base.isEmpty()) {
            return ImageUtilities.addToolTipToImage(icon, "<html>" + segment + "</html>");
        }
        
        // Use the same robust deduplication logic
        String cleanExisting = deduplicateTooltip(base);
        
        // CRITICAL: Remove our own previous segment if it exists to prevent duplication
        cleanExisting = cleanExisting.replaceAll("(?i)<img[^>]*anahata_16\\.png[^>]*>.*", "");
        cleanExisting = cleanExisting.replaceAll("(?i)Not in context in any session.*", "");
        
        String separator = "<br><hr>";
        return ImageUtilities.addToolTipToImage(icon, "<html>" + cleanExisting + separator + segment + "</html>");
    }

    /**
     * Strips HTML tags and deduplicates lines in a tooltip string.
     */
    private String deduplicateTooltip(String tooltip) {
        if (tooltip == null) return null;
        String text = tooltip.replaceAll("(?i)<html>", "").replaceAll("(?i)</html>", "");
        String[] lines = text.split("(?i)<br/?>|\\n|<p/?>|<hr/?>");
        Set<String> uniqueLines = new java.util.LinkedHashSet<>();
        Set<String> seenPlain = new java.util.HashSet<>();
        
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

    /**
     * {@inheritDoc}
     */
    @Override
    public void addChangeListener(ChangeListener cl) {
        listeners.add(ChangeListener.class, cl);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeChangeListener(ChangeListener cl) {
        listeners.remove(ChangeListener.class, cl);
    }

    /**
     * {@inheritDoc}
     * Notifies all registered listeners that the project icons need to be refreshed.
     */
    @Override
    public void stateChanged(ChangeEvent e) {
        Object[] l = listeners.getListenerList();
        for (int i = l.length - 2; i >= 0; i -= 2) {
            if (l[i] == ChangeListener.class) {
                ((ChangeListener) l[i + 1]).stateChanged(e);
            }
        }
    }
    
    /**
     * Triggers a global refresh of all project icons by notifying all 
     * {@link AnahataProjectIconAnnotator} instances.
     * 
     * @param project The project to refresh. (Currently ignored as we refresh all).
     */
    public static void fireRefresh(Project project) {
        LOG.info("Firing global project icon refresh.");
        
        SwingUtilities.invokeLater(() -> {
            // Notify the ProjectIconAnnotator listeners (NetBeans internal)
            for (ProjectIconAnnotator pia : Lookup.getDefault().lookupAll(ProjectIconAnnotator.class)) {
                if (pia instanceof AnahataProjectIconAnnotator apa) {
                    apa.stateChanged(new ChangeEvent(apa));
                }
            }
        });
    }

    /**
     * Performs a comprehensive refresh of both the project icon and the 
     * project root's file annotations (e.g., the [n] session count suffix).
     * 
     * @param project The project to refresh. Can be null for a global refresh.
     */
    public static void fireRefreshAll(Project project) {
        fireRefresh(project);
        if (project != null) {
            FileObject root = project.getProjectDirectory();
            // Force a recursive refresh of the physical file hierarchy to update Files/Favorites views
            uno.anahata.asi.nb.tools.files.nb.FilesContextActionLogic.fireRefreshRecursive(root);
        }
    }
}
