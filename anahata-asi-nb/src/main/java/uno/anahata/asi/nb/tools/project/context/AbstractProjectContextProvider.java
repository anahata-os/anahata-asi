/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.context;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.project.Project;
import org.openide.filesystems.FileObject;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.nb.tools.files.nb.FilesContextActionLogic;

/**
 * Common base class for context providers that are bound to a specific NetBeans project.
 * Centralizes project resolution, toolkit access, and IDE UI synchronization logic.
 * 
 * @author anahata
 */
@Slf4j
public abstract class AbstractProjectContextProvider extends BasicContextProvider {

    /** The parent Projects toolkit instance. */
    protected final Projects projectsToolkit;
    
    /** The absolute canonical path to the project root. */
    @Getter
    protected final String projectPath;

    /** 
     * The cached NetBeans project instance. 
     * Marked transient as it cannot be serialized directly.
     */
    protected transient Project project;

    /**
     * Constructs a new project-bound context provider.
     * 
     * @param id The unique identifier for this provider.
     * @param name The human-readable name.
     * @param description A brief description of the provided context.
     * @param projectsToolkit The parent Projects toolkit.
     * @param projectPath The absolute path to the project.
     */
    public AbstractProjectContextProvider(String id, String name, String description, Projects projectsToolkit, String projectPath) {
        super(id, name, description);
        this.projectsToolkit = projectsToolkit;
        this.projectPath = projectPath;
    }

    /**
     * Resolves the NetBeans Project instance, restoring it from the path if needed.
     * 
     * @return The Project instance, or null if the project is no longer open.
     */
    public Project getProject() {
        if (project == null) {
            try {
                project = Projects.findOpenProject(projectPath);
            } catch (Exception e) {
                log.debug("Project no longer open at path: {}", projectPath);
            }
        }
        return project;
    }

    /**
     * Toggles the providing status and triggers a recursive IDE visual refresh.
     * <p>
     * Implementation details:
     * When the state changes, it identifies the project's root FileObject and 
     * notifies the IDE that icons and names in the project tree should be 
     * redrawn to reflect the current context state.
     * </p>
     * 
     * @param enabled The new activation state.
     */
    @Override
    public void setProviding(boolean enabled) {
        boolean old = isProviding();
        super.setProviding(enabled);
        
        if (old != enabled) {
            Project p = getProject();
            if (p != null) {
                FileObject root = p.getProjectDirectory();
                if (root != null) {
                    FilesContextActionLogic.fireRefreshRecursive(root);
                }
            }
        }
    }
}
