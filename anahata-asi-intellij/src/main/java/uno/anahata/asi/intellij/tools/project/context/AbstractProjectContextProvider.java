/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.project.context;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.context.BasicContextProvider;
import uno.anahata.asi.intellij.tools.project.Projects;

import java.nio.file.Path;

/**
 * Common base class for context providers that are bound to a specific IntelliJ project.
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
     * The cached IntelliJ project instance. 
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
     * Resolves the IntelliJ Project instance, restoring it from the path if needed.
     * 
     * @return The Project instance, or null if the project is no longer open.
     */
    public Project getProject() {
        if (project == null) {
            for (Project p : ProjectManager.getInstance().getOpenProjects()) {
                String basePath = p.getBasePath();
                if (basePath != null && Path.of(basePath).toAbsolutePath().toString().equals(projectPath)) {
                    project = p;
                    break;
                }
            }
        }
        return project;
    }
}