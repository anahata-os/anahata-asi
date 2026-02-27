/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.nb;

import java.awt.Image;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
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
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.nb.tools.project.Projects;

/**
 * Master Aggregator for NetBeans Project icons.
 * <p>
 * This annotator implements chained delegation to preserve IDE metadata (like Git branch names)
 * while overlaying Anahata's session badges and unified context totals.
 * </p>
 * 
 * @author anahata
 */
//@ServiceProvider(service = ProjectIconAnnotator.class, position = 2000)
public class AnahataProjectIconAnnotator implements ProjectIconAnnotator, ChangeListener {
    private static final Logger LOG = Logger.getLogger(AnahataProjectIconAnnotator.class.getName());
    
    private static final String BADGE_ICON_PATH = "icons/v2/anahata.png";
    private static final Image BADGE;
    
    private final javax.swing.event.EventListenerList listeners = new javax.swing.event.EventListenerList();

    static {
        Image img = ImageUtilities.loadImage(BADGE_ICON_PATH);
        BADGE = (img != null) ? img.getScaledInstance(8, 8, Image.SCALE_SMOOTH) : null;
    }

    /**
     * Default constructor for the project annotator.
     */
    public AnahataProjectIconAnnotator() {
        LOG.log(Level.INFO, "AnahataProjectIconAnnotator (Universal Chain) initialized.");
    }
    
    /**
     * Annotates the project icon, delegating to other providers first to ensure Git info is preserved.
     * 
     * @param p The project to annotate.
     * @param icon The base icon.
     * @param opened Whether the project is opened.
     * @return The aggregated and badged icon.
     */
    @Override
    public Image annotateIcon(Project p, Image icon, boolean opened) {
        String existingTooltip = ImageUtilities.getImageToolTip(icon);
        
        String projectPath = p.getProjectDirectory().getPath();
        List<Agi> activeAgis = AnahataInstaller.getContainer().getActiveAgis();
        
        List<String> tooltipLines = new ArrayList<>();

        for (Agi agi : activeAgis) {
            final int[] total = {0};
            agi.getToolManager().getToolkitInstance(Projects.class).ifPresent(projectsTool -> {
                projectsTool.getProjectProvider(projectPath).ifPresent(pcp -> {
                    List<String> activeChildren = pcp.getChildrenProviders().stream()
                                .filter(ContextProvider::isProviding)
                                .map(ContextProvider::getName)
                                .collect(Collectors.toList());
                    
                    total[0] += activeChildren.size();
                    
                    // Round 10: In the Project node, we only count providers. 
                    // Root files are listed in the tooltip but don't bloat the main total.
                    int fileCount = uno.anahata.asi.nb.tools.files.nb.FilesContextActionLogic.getSessionFileCounts(p.getProjectDirectory(), false).getOrDefault(agi, 0);
                    
                    if (total[0] > 0 || fileCount > 0) {
                        String line = "<b>" + agi.getDisplayName() + "</b> [" + total[0] + "]";
                        if (!activeChildren.isEmpty()) {
                            line += ": " + String.join(", ", activeChildren);
                        }
                        if (fileCount > 0) {
                            line += " (" + fileCount + " root files)";
                        }
                        tooltipLines.add(line);
                    }
                });
            });
        }

        if (tooltipLines.isEmpty()) {
            return icon;
        }

        StringBuilder sb = new StringBuilder();
        sb.append("<img src=\"").append(getClass().getResource("/icons/anahata_16.png")).append("\" width=\"12\" height=\"12\"> ");
        sb.append("<b>In Context in:</b><br>");
        for (String line : tooltipLines) {
            sb.append("&nbsp;&nbsp;&bull;&nbsp;").append(line).append("<br>");
        }
        
        Image badgedIcon = (BADGE != null) ? ImageUtilities.mergeImages(icon, BADGE, 8, 0) : icon;
        return mergeTooltip(badgedIcon, sb.toString(), existingTooltip);
    }

    /**
     * Merges the Anahata tooltip segment with existing tooltips.
     * 
     * @param icon The target icon.
     * @param segment Our HTML segment.
     * @param existing Pre-captured existing tooltip.
     * @return Icon with merged tooltip.
     */
    private Image mergeTooltip(Image icon, String segment, String existing) {
        String base = (existing != null && !existing.isEmpty()) ? existing : "";
        if (base.toLowerCase().contains("<html>")) {
            base = base.replaceAll("(?i)</?html>", "");
        }
        
        // Remove old Anahata markers to prevent stuttering
        base = base.replaceAll("(?i)<img[^>]*anahata_16\\.png[^>]*>.*", "");
        
        String separator = (!base.isEmpty()) ? "<br><hr>" : "";
        return ImageUtilities.addToolTipToImage(icon, "<html>" + base + separator + segment + "</html>");
    }

    /** {@inheritDoc} */
    @Override
    public void addChangeListener(ChangeListener cl) {
        listeners.add(ChangeListener.class, cl);
    }

    /** {@inheritDoc} */
    @Override
    public void removeChangeListener(ChangeListener cl) {
        listeners.remove(ChangeListener.class, cl);
    }

    /**
     * Notifies listeners to refresh project icons.
     * 
     * @param e The change event.
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
     * Triggers a global refresh of the projects tab icons.
     */
    public static void fireRefresh() {
        SwingUtilities.invokeLater(() -> {
            for (ProjectIconAnnotator pia : Lookup.getDefault().lookupAll(ProjectIconAnnotator.class)) {
                if (pia instanceof AnahataProjectIconAnnotator apa) {
                    apa.stateChanged(new ChangeEvent(apa));
                }
            }
        });
    }

    /**
     * Comprehensive refresh for both logical project icons and physical file tree annotations.
     * 
     * @param project The project to refresh (can be null).
     */
    public static void fireRefreshAll(Project project) {
        /*
        fireRefresh();
        if (project != null) {
            FileObject root = project.getProjectDirectory();
            uno.anahata.asi.nb.tools.files.nb.FilesContextActionLogic.fireRefreshRecursive(root);
        }
*/
    }
}
