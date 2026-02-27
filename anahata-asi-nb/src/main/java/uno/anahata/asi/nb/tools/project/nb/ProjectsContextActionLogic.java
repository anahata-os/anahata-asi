/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.project.nb;

import java.util.logging.Logger;
import org.netbeans.api.project.Project;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.nb.tools.project.Projects;

/**
 * Stateless utility class containing the core logic for managing NetBeans 
 * Projects within an AI agi context.
 * 
 * @author anahata
 */
public class ProjectsContextActionLogic {

    private static final Logger LOG = Logger.getLogger(ProjectsContextActionLogic.class.getName());

    /**
     * Checks if a specific project is currently active in the given agi's context.
     * 
     * @param project The project to check.
     * @param agi The agi session to check against.
     * @return {@code true} if the project is in context.
     */
    public static boolean isProjectInContext(Project project, Agi agi) {
        return agi.getToolManager().getToolkitInstance(Projects.class)
                .flatMap(pt -> pt.getProjectProvider(project.getProjectDirectory().getPath()))
                .map(pcp -> pcp.isProviding())
                .orElse(false);
    }

    /**
     * Counts how many active agi sessions have the given project in their context.
     * 
     * @param project The project to check.
     * @return The number of agis containing the project.
     */
    public static int countAgisProjectInContext(Project project) {
        int count = 0;
        for (Agi agi : AnahataInstaller.getContainer().getActiveAgis()) {
            if (isProjectInContext(project, agi)) {
                count++;
            }
        }
        return count;
    }
}
