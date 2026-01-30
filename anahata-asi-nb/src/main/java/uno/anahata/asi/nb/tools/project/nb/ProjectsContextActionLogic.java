/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.project.nb;

import java.util.logging.Logger;
import org.netbeans.api.project.Project;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.nb.tools.project.Projects;

/**
 * Stateless utility class containing the core logic for managing NetBeans 
 * Projects within an AI chat context.
 * 
 * @author anahata
 */
public class ProjectsContextActionLogic {

    private static final Logger LOG = Logger.getLogger(ProjectsContextActionLogic.class.getName());

    /**
     * Checks if a specific project is currently active in the given chat's context.
     * 
     * @param project The project to check.
     * @param chat The chat session to check against.
     * @return {@code true} if the project is in context.
     */
    public static boolean isProjectInContext(Project project, Chat chat) {
        return chat.getToolManager().getToolkitInstance(Projects.class)
                .flatMap(pt -> pt.getProjectProvider(project.getProjectDirectory().getPath()))
                .map(pcp -> pcp.isProviding())
                .orElse(false);
    }

    /**
     * Counts how many active chat sessions have the given project in their context.
     * 
     * @param project The project to check.
     * @return The number of chats containing the project.
     */
    public static int countChatsProjectInContext(Project project) {
        int count = 0;
        for (Chat chat : AnahataInstaller.getContainer().getActiveChats()) {
            if (isProjectInContext(project, chat)) {
                count++;
            }
        }
        return count;
    }
}
