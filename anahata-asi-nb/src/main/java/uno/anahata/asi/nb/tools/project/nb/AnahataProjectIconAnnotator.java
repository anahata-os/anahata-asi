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
        String projectPath = p.getProjectDirectory().getPath();
        List<Chat> activeChats = AnahataInstaller.getContainer().getActiveChats();
        
        List<String> tooltipLines = new ArrayList<>();

        for (Chat chat : activeChats) {
            chat.getToolManager().getToolkitInstance(Projects.class).ifPresent(projectsTool -> {
                projectsTool.getProjectProvider(projectPath).ifPresent(pcp -> {
                    if (pcp.isProviding()) {
                        List<String> activeChildren = pcp.getChildrenProviders().stream()
                                .filter(ContextProvider::isProviding)
                                .map(ContextProvider::getName)
                                .collect(Collectors.toList());
                        
                        String line = "<b>" + chat.getDisplayName() + "</b>";
                        if (!activeChildren.isEmpty()) {
                            line += ": " + String.join(", ", activeChildren);
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
                // Offset 16 is the right place because other badges (like the warning icon) 
                // show at (8,0). This places our badge to the right of the main 16x16 icon.
                Image badgedIcon = ImageUtilities.mergeImages(icon, BADGE, 16, 0);
                return mergeTooltip(badgedIcon, sb.toString());
            }
        } else {
            sb.append("Not in context in any session");
            return mergeTooltip(icon, sb.toString());
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
        String separator = "<hr>";
        
        if (cleanExisting.toLowerCase().contains("<html>")) {
            return ImageUtilities.addToolTipToImage(icon, cleanExisting.replaceFirst("(?i)</html>", separator + segment + "</html>"));
        }
        
        // Otherwise, wrap both in HTML
        return ImageUtilities.addToolTipToImage(icon, "<html>" + cleanExisting + separator + segment + "</html>");
    }

    /**
     * Strips HTML tags and deduplicates lines in a tooltip string.
     * 
     * @param tooltip The raw tooltip string.
     * @return A cleaned, deduplicated string.
     */
    private String deduplicateTooltip(String tooltip) {
        if (tooltip == null) {
            return null;
        }
        String text = tooltip.replaceAll("(?i)<html>", "").replaceAll("(?i)</html>", "");
        String[] lines = text.split("<br>|\n");
        Set<String> uniqueLines = new LinkedHashSet<>();
        for (String line : lines) {
            String trimmed = line.trim();
            if (!trimmed.isEmpty()) {
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
            try {
                FileAnnotationProvider.fireRefresh(root.getFileSystem(), Collections.singleton(root));
            } catch (IOException ex) {
                LOG.log(Level.WARNING, "Failed to refresh project root annotations", ex);
            }
        }
    }
}
