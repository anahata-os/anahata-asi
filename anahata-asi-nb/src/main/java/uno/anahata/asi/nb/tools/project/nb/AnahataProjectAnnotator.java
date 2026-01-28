/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.nb;

import java.awt.Image;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ProjectIconAnnotator;
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;
import org.openide.util.lookup.ServiceProvider;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.nb.tools.project.context.ProjectContextProvider;

/**
 * Annotates project icons in the NetBeans Projects tab with an Anahata badge
 * if the project is currently active in any AI chat context.
 * <p>
 * This class implements {@link ProjectIconAnnotator} to provide visual feedback
 * in the IDE's project explorer.
 * 
 * @author anahata
 */
@ServiceProvider(service = ProjectIconAnnotator.class)
public class AnahataProjectAnnotator implements ProjectIconAnnotator, ChangeListener {
    private static final Logger LOG = Logger.getLogger(AnahataProjectAnnotator.class.getName());
    
    /** The badge image to overlay on project icons. */
    private static final Image BADGE;
    
    static {
        LOG.info("AnahataProjectAnnotator class loaded.");
        Image img = ImageUtilities.loadImage("icons/anahata.png");
        if (img != null) {
            BADGE = img.getScaledInstance(8, 8, Image.SCALE_SMOOTH);
        } else {
            BADGE = null;
        }
    }

    /** The list of change listeners for icon refresh. */
    private final javax.swing.event.EventListenerList listeners = new javax.swing.event.EventListenerList();

    /**
     * Default constructor for the annotator.
     */
    public AnahataProjectAnnotator() {
        LOG.log(Level.INFO, "AnahataProjectAnnotator instance created: {0}", System.identityHashCode(this));
    }
    
    /**
     * {@inheritDoc}
     * Annotates the project icon with a badge if the project is in an active AI context.
     */
    @Override
    public Image annotateIcon(Project p, Image icon, boolean opened) {
        if (isProjectInContext(p)) {
            if (BADGE != null) {
                // Use offset 16 to place the badge to the right of the 16x16 icon.
                // This avoids clashing with 'project problems' and other decorations.
                return ImageUtilities.mergeImages(icon, BADGE, 16, 0);
            }
        }
        return icon;
    }

    /**
     * Checks if the given project is currently active in any AI chat context.
     * 
     * @param p The project to check.
     * @return {@code true} if the project is in context, {@code false} otherwise.
     */
    private boolean isProjectInContext(Project p) {
        String path = p.getProjectDirectory().getPath();
        for (Chat chat : AnahataInstaller.getContainer().getActiveChats()) {
            Optional<Projects> projectsTool = chat.getToolManager().getToolkitInstance(Projects.class);
            if (projectsTool.isPresent()) {
                boolean providing = projectsTool.get().getChildrenProviders().stream()
                    .filter(cp -> cp instanceof ProjectContextProvider)
                    .map(cp -> (ProjectContextProvider) cp)
                    .filter(pcp -> pcp.getProjectPath().equals(path))
                    .anyMatch(pcp -> pcp.isProviding());
                if (providing) {
                    LOG.log(Level.FINE, "Project {0} is active in chat: {1}", new Object[]{path, chat.getDisplayName()});
                    return true;
                }
            }
        }
        return false;
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
     * {@link AnahataProjectAnnotator} instances.
     * 
     * @param project The project to refresh (currently unused, but kept for API compatibility).
     */
    public static void fireRefresh(Project project) {
        LOG.info("Firing global project icon refresh.");
        
        // Note: We don't fire FileStatusEvent here because fireFileStatusChanged is protected 
        // in FileSystem. Instead, we rely on the ChangeListeners registered with the annotators.
        
        for (ProjectIconAnnotator pia : Lookup.getDefault().lookupAll(ProjectIconAnnotator.class)) {
            if (pia instanceof AnahataProjectAnnotator apa) {
                apa.stateChanged(new ChangeEvent(apa));
            }
        }
    }
}
