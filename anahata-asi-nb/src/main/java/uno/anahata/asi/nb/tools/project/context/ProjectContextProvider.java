/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.context;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.nb.tools.project.Projects;

/**
 * A hierarchical context provider for a specific NetBeans project.
 * It aggregates the {@link ProjectOverviewContextProvider} and the 
 * {@link AnahataMdContextProvider} into a single project-scoped node.
 * <p>
 * The project name is fetched dynamically to ensure it remains current.
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

    /**
     * Constructs a new project context provider.
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
        
        // Register with parent
        this.setParent(projectsToolkit);
        
        // Initialize children
        ProjectOverviewContextProvider overview = new ProjectOverviewContextProvider(projectsToolkit, projectPath);
        overview.setParent(this);
        children.add(overview);
        
        AnahataMdContextProvider anahataMd = new AnahataMdContextProvider(projectsToolkit, projectPath);
        anahataMd.setParent(this);
        children.add(anahataMd);
    }

    /**
     * Gets the NetBeans project instance, restoring it from the path if necessary.
     * 
     * @return The project instance, or null if it cannot be found.
     */
    public Project getProject() {
        if (project == null) {
            try {
                project = Projects.findOpenProject(projectPath);
            } catch (Exception e) {
                log.error("Failed to restore project reference for path: {}", projectPath, e);
            }
        }
        return project;
    }

    /**
     * {@inheritDoc}
     * Dynamically returns the current display name of the project.
     */
    @Override
    public String getName() {
        Project p = getProject();
        return p != null ? ProjectUtils.getInformation(p).getDisplayName() : super.getName();
    }
}
