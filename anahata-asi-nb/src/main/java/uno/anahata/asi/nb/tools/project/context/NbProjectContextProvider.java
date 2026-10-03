/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.project.context;

import java.nio.file.Path;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.nb.tools.project.NbProjects;
import uno.anahata.asi.nb.tools.project.alerts.JavacAlert;
import uno.anahata.asi.nb.tools.project.alerts.ProjectAlert;
import uno.anahata.asi.nb.tools.project.alerts.ProjectDiagnostics;
import uno.anahata.asi.nb.tools.project.components.ProjectStructure;
import uno.anahata.asi.toolkit.project.AbstractProjectContextProvider;
import uno.anahata.asi.toolkit.project.ProjectOverview;
import uno.anahata.asi.toolkit.project.ProjectStructureScope;

/**
 * Unified, comprehensive context provider for a NetBeans project.
 * <p>
 * Combines Overview, Alerts (compiler errors and diagnostics), and Structure (AST source trees,
 * root files, resources) into a single cohesive provider node. Adheres to NetBeans's flat
 * project workspace topology while inheriting workspace-level settings from the parent
 * {@link NbProjects} toolkit.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class NbProjectContextProvider extends AbstractProjectContextProvider {

    /**
     * The NetBeans project instance.
     */
    protected transient Project project;

    /**
     * Resolves the NetBeans Project instance, restoring it from the path if needed after deserialization.
     *
     * @return The Project instance, or null if the project is no longer open.
     */
    public Project getProject() {
        if (project == null && projectPath != null) {
            try {
                project = NbProjects.findOpenProject(projectPath);
            } catch (Exception e) {
                log.warn("Project no longer open or resolvable at path: {}", projectPath);
            }
        }
        return project;
    }

    /**
     * Constructs a new unified project context provider.
     *
     * @param projectsToolkit The parent Projects toolkit.
     * @param project The NetBeans project instance.
     */
    public NbProjectContextProvider(NbProjects projectsToolkit, Project project) {
        super(NbProjects.getCanonicalPath(project.getProjectDirectory()),
                ProjectUtils.getInformation(project).getDisplayName(),
                "Unified Project Context Provider for: " + ProjectUtils.getInformation(project).getDisplayName(),
                projectsToolkit,
                NbProjects.getCanonicalPath(project.getProjectDirectory())
        );
        this.project = project;

        syncMdResource();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the dynamic display name of the NetBeans project as retrieved from the project information,
     * falling back to the persisted display name if the project is not currently resolved.
     * </p>
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
     * {@inheritDoc}
     * <p>
     * Builds and returns the structured {@link ProjectOverview} model for this project.
     * </p>
     */
    @Override
    public ProjectOverview getOverview() {
        try {
            return projectsToolkit.getOverview(projectPath);
        } catch (Exception e) {
            log.error("Failed to build ProjectOverview for: " + projectPath, e);
            Project p = getProject();
            return ProjectOverview.builder()
                    .id(p != null ? p.getProjectDirectory().getNameExt() : (projectPath != null ? Path.of(projectPath).getFileName().toString() : "unknown"))
                    .displayName(getName())
                    .projectDirectory(projectPath)
                    .build();
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Unified RAG message generator: emits the Project Overview,
     * compiler alerts (if enabled in scope), and project AST structure.
     * </p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        Project p = getProject();
        if (p == null) {
            ragMessage.addTextPart("Couldn't fetch NetBeans Project, project may have been closed");
            return;
        }

        StringBuilder sb = new StringBuilder();
        ProjectStructureScope effectiveScope = getEffectiveScope();

        // 1. Overview Section
        ProjectOverview overview = getOverview();
        sb.append(overview.toMarkdown()).append("\n");

        // 2. Alerts Section
        if (effectiveScope.isShowAlerts()) {
            appendAlerts(sb);
        }

        // 3. Structure Section
        if (this.scope != null) {
            sb.append("\n> [!NOTE]\n> Project Structure Scope Override: `").append(this.scope).append("`\n\n");
        } else {
            sb.append("\n> [!NOTE]\n> Project Structure Scope: Inherited from default (`").append(effectiveScope).append("`)\n\n");
        }
        ProjectStructure structure = new ProjectStructure(p, effectiveScope);
        structure.renderMarkdown(sb, "  ", effectiveScope);

        ragMessage.addTextPart(sb.toString().trim());
    }

    /**
     * Appends compiler errors and high-level project problems if any are present.
     *
     * @param sb The target StringBuilder.
     */
    private void appendAlerts(StringBuilder sb) {
        try {
            ProjectDiagnostics diags = ((NbProjects) projectsToolkit).getProjectAlerts(projectPath);
            if (diags.getJavacAlerts().isEmpty() && diags.getProjectAlerts().isEmpty()) {
                sb.append("\n**Project Alerts ").append(diags.getProjectName()).append("**: No alerts found.\n");
            } else {
                sb.append("\n🚨🚨🚨 **CRITICAL COMPILER ALERTS & PROJECT ERRORS [").append(diags.getProjectName()).append("]** 🚨🚨🚨\n");
                sb.append("> [!CAUTION]\n");

                if (!diags.getProjectAlerts().isEmpty()) {
                    sb.append("> ### ⚠️ Project Problems (High-Level)\n");
                    for (ProjectAlert alert : diags.getProjectAlerts()) {
                        sb.append("> - **[").append(alert.getSeverity()).append("]** ")
                          .append(alert.getDisplayName()).append(": ").append(alert.getDescription().replace("\n", " ")).append("\n");
                    }
                }

                if (!diags.getJavacAlerts().isEmpty()) {
                    sb.append("> ### 🛑 Java Compiler Errors (File-Level)\n");
                    for (JavacAlert alert : diags.getJavacAlerts()) {
                        sb.append("> - 🛑 **[").append(alert.getKind()).append("]** `")
                          .append(alert.getFilePath()).append(":").append(alert.getLineNumber())
                          .append("`: ").append(alert.getMessage().replace("\n", " ")).append("\n");
                    }
                }
                sb.append("\n");
            }
        } catch (Exception e) {
            log.error("Failed to query alerts for: " + projectPath, e);
        }
    }
}
