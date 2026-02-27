/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.context;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.nb.tools.project.alerts.ProjectAlertsContextProvider;
import uno.anahata.asi.nb.tools.files.nb.FilesContextActionLogic;

/**
 * A hierarchical context provider for a specific NetBeans project.
 * It aggregates specialized providers like {@link ProjectOverviewContextProvider}, 
 * {@link AnahataMdContextProvider}, {@link ProjectFilesContextProvider}, and {@link ProjectAlertsContextProvider}
 * into a single project-scoped node.
 * 
 * @author anahata
 */
@Slf4j
public class ProjectContextProvider extends BasicContextProvider {

    /** The NetBeans project instance. Transient to support serialization. */
    private transient Project project;

    /** The absolute path to the project directory. */
    @Getter
    private final String projectPath;
    
    /** The unique icon ID for this project. */
    private final String iconId;

    /**
     * Constructs a new project context provider.
     * <p>
     * Implementation details:
     * Initializes all project-specific child providers (Overview, anahata.md, Files, Components, Alerts) 
     * and sets this provider as their parent.
     * </p>
     * 
     * @param projectsToolkit The parent Projects toolkit.
     * @param project The NetBeans project instance.
     */
    public ProjectContextProvider(Projects projectsToolkit, Project project) {
        super(project.getProjectDirectory().getPath(), 
              ProjectUtils.getInformation(project).getDisplayName(), 
              "Context for project: " + ProjectUtils.getInformation(project).getDisplayName());
        this.parent = projectsToolkit;
        this.project = project;
        this.projectPath = project.getProjectDirectory().getPath();
        this.iconId = "nb.project." + projectPath;
        
        // Register with parent
        this.setParent(projectsToolkit);
        
        // Initialize children
        ProjectOverviewContextProvider overview = new ProjectOverviewContextProvider(projectsToolkit, projectPath);
        overview.setParent(this);
        children.add(overview);
        
        AnahataMdContextProvider anahataMd = new AnahataMdContextProvider(projectsToolkit, projectPath);
        anahataMd.setParent(this);
        children.add(anahataMd);

        ProjectFilesContextProvider files = new ProjectFilesContextProvider(projectsToolkit, projectPath);
        files.setParent(this);
        children.add(files);

        ProjectComponentsContextProvider components = new ProjectComponentsContextProvider(projectsToolkit, projectPath);
        components.setParent(this);
        children.add(components);

        ProjectAlertsContextProvider alerts = new ProjectAlertsContextProvider(projectsToolkit, projectPath);
        alerts.setParent(this);
        children.add(alerts);
    }

    /**
     * Gets the NetBeans project instance, restoring it from the path if necessary.
     * <p>
     * Implementation details:
     * If the transient project field is null, it attempts to find it using the toolkit's 
     * canonical path resolution.
     * </p>
     * 
     * @return The project instance, or null if it cannot be found or is closed.
     */
    public Project getProject() {
        if (project == null) {
            try {
                project = Projects.findOpenProject(projectPath);
            } catch (Exception e) {
                // Silent if project is simply closed
                log.debug("Project not open at path: {}", projectPath);
            }
        }
        return project;
    }

    /**
     * Returns the dynamic display name of the project.
     * <p>
     * Implementation details:
     * Resolves the current display name from the NetBeans project information.
     * </p>
     * 
     * @return The project's display name.
     */
    @Override
    public String getName() {
        Project p = getProject();
        if (p != null) {
            return ProjectUtils.getInformation(p).getDisplayName();
        }
        return super.getName();
    }

    /** 
     * Returns the unique icon ID for the project node.
     * <p>
     * Implementation details:
     * Returns a string prefixed with 'nb.project.' followed by the path.
     * </p>
     * 
     * @return The icon ID string.
     */
    @Override
    public String getIconId() {
        return iconId;
    }

    /**
     * Toggles the providing state and triggers a project-level UI refresh.
     * <p>
     * Implementation details:
     * Updates the base state and calls {@link FilesContextActionLogic#fireRefreshRecursive} 
     * for the project directory to update icons in the Projects tab.
     * </p>
     * 
     * @param enabled The new state.
     */
    @Override
    public void setProviding(boolean enabled) {
        boolean old = isProviding();
        super.setProviding(enabled);
        if (old != enabled) {
            Project p = getProject();
            if (p != null) {
                FilesContextActionLogic.fireRefreshRecursive(p.getProjectDirectory());
            }
        }
    }
}
