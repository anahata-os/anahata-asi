/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.project;

import java.util.List;
import java.util.Optional;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;

/**
 * Universal base toolkit for IDE Project APIs across all host environments.
 * <p>
 * Manages workspace-wide default project structure granularity settings and common
 * project operations, standardizing project management capabilities between NetBeans,
 * IntelliJ IDEA, and standalone platforms.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Universal toolkit for project lifecycle, environment, and structure inspection.")
public abstract class AbstractProjects extends AnahataToolkit {

    /**
     * Default project structure granularity scope inherited by project context providers.
     */
    @Getter
    protected ProjectStructureScope defaultScope = new ProjectStructureScope();

    /**
     * Sets the workspace-wide default project structure granularity scope.
     * Fires a {@code "projectStructureScope"} property change event.
     *
     * @param defaultScope The new default scope.
     */
    public void setDefaultScope(ProjectStructureScope defaultScope) {
        ProjectStructureScope old = this.defaultScope;
        this.defaultScope = defaultScope != null ? defaultScope : new ProjectStructureScope();
        propertyChangeSupport.firePropertyChange("projectStructureScope", old, this.defaultScope);
    }

    /**
     * Configures the project structure granularity scope for a specific project or module.
     *
     * @param projectPath The absolute path of the project or module.
     * @param scope The new granularity scope settings, or null to inherit from default.
     * @throws AgiToolException if the project provider is not found.
     */
    @AgiTool("Configures the project structure granularity scope for a specific project or module.")
    public void setProjectStructureScope(
            @AgiToolParam(value = "The absolute path of the project or module.", rendererId = "path") String projectPath,
            @AgiToolParam(value = "The new granularity scope settings, or null to inherit from default.", required = false) ProjectStructureScope scope) throws AgiToolException {
        AbstractProjectContextProvider pcp = getProjectProvider(projectPath)
                .orElseThrow(() -> new AgiToolException("Project context provider not found for: " + projectPath));
        pcp.setScope(scope);
        log.info("Updated ProjectStructureScope for {}: {}", projectPath, scope != null ? scope : "INHERITED");
    }

    /**
     * Returns a list of absolute paths of all currently open projects in the IDE.
     *
     * @return List of project base directory paths.
     */
    public abstract List<String> getOpenProjects();

    /**
     * Closes one or more open projects in the IDE.
     *
     * @param projectPaths A list of absolute paths of the projects to close.
     * @throws Exception if project closing fails.
     */
    public abstract void closeProjects(List<String> projectPaths) throws Exception;

    /**
     * Opens a project directory programmatically in the IDE.
     *
     * @param projectPath The absolute path of the project directory to open.
     * @return A status message describing the outcome.
     * @throws Exception if project opening fails.
     */
    public abstract String openProject(String projectPath) throws Exception;

    /**
     * Generates a structured {@link ProjectOverview} model for a specific project directory.
     *
     * @param projectPath The absolute path of the project.
     * @return The populated ProjectOverview.
     * @throws Exception if overview generation fails.
     */
    public abstract ProjectOverview getOverview(String projectPath) throws Exception;

    /**
     * Resolves the active project context provider for a given project path.
     *
     * @param projectPath The absolute path of the project.
     * @return An Optional containing the matching provider.
     */
    public abstract Optional<? extends AbstractProjectContextProvider> getProjectProvider(String projectPath);

    /**
     * Enables or disables the project context provider for a specific project or module directory.
     *
     * @param projectPath The absolute path of the project or module.
     * @param enabled Whether to enable the context provider.
     */
    @AgiTool("Enables or disables the project context provider (overview and anahata.md) for a specific project or module directory.")
    public void setProjectProviderEnabled(
            @AgiToolParam(value = "The absolute path of the project or module.", rendererId = "path") String projectPath,
            @AgiToolParam("Whether to enable the context provider.") boolean enabled) {
        getProjectProvider(projectPath).ifPresent(pcp -> {
            pcp.setProviding(enabled);
            log.info("Project context for {} set to: {}", projectPath, enabled);
        });
    }
}
