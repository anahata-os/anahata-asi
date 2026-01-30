/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.nb;

import java.awt.Image;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ProjectIconAnnotator;
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;
import org.openide.util.lookup.ServiceProvider;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.nb.tools.project.Projects;

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
public class AnahataProjectAnnotator implements ProjectIconAnnotator, ChangeListener {
    private static final Logger LOG = Logger.getLogger(AnahataProjectAnnotator.class.getName());
    
    /** The badge image to overlay on project icons. */
    private static final Image BADGE;
    
    /** 
     * The list of change listeners for icon refresh. 
     * Shared across all instances to ensure global synchronization with the IDE.
     */
    private static final javax.swing.event.EventListenerList LISTENERS = new javax.swing.event.EventListenerList();

    static {
        LOG.info("AnahataProjectAnnotator class loaded.");
        Image img = ImageUtilities.loadImage("icons/anahata.png");
        if (img != null) {
            BADGE = img.getScaledInstance(8, 8, Image.SCALE_SMOOTH);
        } else {
            BADGE = null;
        }
    }

    /**
     * Default constructor for the annotator.
     */
    public AnahataProjectAnnotator() {
        LOG.log(Level.INFO, "AnahataProjectAnnotator instance created.");
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

        // Build the HTML tooltip
        StringBuilder sb = new StringBuilder("<html>");
        // Use a smaller icon (12x12) in the tooltip
        sb.append("<img src=\"").append(getClass().getResource("/icons/anahata_16.png")).append("\" width=\"12\" height=\"12\"> ");
        
        if (!tooltipLines.isEmpty()) {
            sb.append("In Context in:<br>");
            for (String line : tooltipLines) {
                sb.append("&nbsp;&nbsp;&bull;&nbsp;").append(line).append("<br>");
            }
            
            if (BADGE != null) {
                // Offset 16 is the right place because other badges (like the warning icon) 
                // show at (8,0). This places our badge to the right of the main 16x16 icon.
                Image badgedIcon = ImageUtilities.mergeImages(icon, BADGE, 16, 0);
                return ImageUtilities.addToolTipToImage(badgedIcon, sb.toString());
            }
        } else {
            sb.append("Not in context in any session");
            // No badge, but still add the "Not in context" tooltip to the original icon
            return ImageUtilities.addToolTipToImage(icon, sb.toString());
        }

        return icon;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void addChangeListener(ChangeListener cl) {
        LISTENERS.add(ChangeListener.class, cl);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeChangeListener(ChangeListener cl) {
        LISTENERS.remove(ChangeListener.class, cl);
    }

    /**
     * {@inheritDoc}
     * Notifies all registered listeners that the project icons need to be refreshed.
     */
    @Override
    public void stateChanged(ChangeEvent e) {
        Object[] l = LISTENERS.getListenerList();
        for (int i = l.length - 2; i >= 0; i -= 2) {
            if (l[i] == ChangeListener.class) {
                ((ChangeListener) l[i + 1]).stateChanged(e);
            }
        }
    }
    
    /**
     * Triggers a global refresh of all project icons by notifying all 
     * {@link AnahataProjectAnnotator} instances.
     * 
     * @param project The project to refresh. (Currently ignored as we refresh all).
     */
    public static void fireRefresh(Project project) {
        LOG.info("Firing global project icon refresh.");
        
        SwingUtilities.invokeLater(() -> {
            // Notify the ProjectIconAnnotator listeners (NetBeans internal)
            for (ProjectIconAnnotator pia : Lookup.getDefault().lookupAll(ProjectIconAnnotator.class)) {
                if (pia instanceof AnahataProjectAnnotator apa) {
                    apa.stateChanged(new ChangeEvent(apa));
                }
            }
        });
    }
}
