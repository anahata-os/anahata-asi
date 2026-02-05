/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.context;

import javax.swing.Icon;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.nb.tools.project.alerts.ProjectAlertsContextProvider;
import uno.anahata.asi.nb.tools.project.nb.AnahataProjectIconAnnotator;
import uno.anahata.asi.swing.icons.IconUtils;

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
        
        // Register the authentic project icon in the global registry
        Icon icon = ProjectUtils.getInformation(project).getIcon();
        IconUtils.registerIcon(iconId, icon);
        
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
     * {@inheritDoc}
     * Dynamically returns the current display name of the project.
     */
    @Override
    public String getName() {
        Project p = getProject();
        return p != null ? ProjectUtils.getInformation(p).getDisplayName() : super.getName();
    }

    /** {@inheritDoc} */
    @Override
    public String getIconId() {
        return iconId;
    }

    /**
     * {@inheritDoc}
     * Overridden to trigger a comprehensive project refresh in the IDE.
     */
    @Override
    public void setProviding(boolean enabled) {
        boolean old = isProviding();
        super.setProviding(enabled);
        if (old != enabled) {
            AnahataProjectIconAnnotator.fireRefreshAll(getProject());
        }
    }
}
