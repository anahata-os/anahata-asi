/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.project.alerts;

import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.nb.tools.project.context.ProjectContextProvider;
import uno.anahata.asi.nb.tools.files.nb.FilesContextActionLogic;

/**
 * Provides real-time diagnostics for a project, including Java compiler errors
 * and high-level project problems.
 * 
 * @author anahata-ai
 */
@Slf4j
public class ProjectAlertsContextProvider extends BasicContextProvider {

    /** The parent Projects toolkit. */
    private final Projects projectsToolkit;
    
    /** The absolute path to the project directory. */
    private final String projectPath;

    /**
     * Constructs a new alerts provider for a specific project.
     * 
     * @param projectsToolkit The parent Projects toolkit.
     * @param projectPath The absolute path to the project.
     */
    public ProjectAlertsContextProvider(Projects projectsToolkit, String projectPath) {
        super("alerts", "Project Alerts", "Compiler errors and project problems");
        this.projectsToolkit = projectsToolkit;
        this.projectPath = projectPath;
        // Enabled by default for better visibility of compile issues
        setProviding(true);
    }

    /**
     * Injects project diagnostics into the RAG message.
     * <p>
     * Implementation details:
     * Fetches current alerts from the Projects toolkit and appends them to the 
     * RAG message. Alerts are grouped by type (Project vs. Compiler).
     * </p>
     * 
     * @param ragMessage The target RAG message.
     * @throws Exception if diagnostics cannot be retrieved.
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        ProjectDiagnostics diags = projectsToolkit.getProjectAlerts(projectPath);
        
        StringBuilder sb = new StringBuilder();
        sb.append("\n## Project Alerts (").append(diags.getProjectName()).append(")\n");
        
        if (diags.getJavacAlerts().isEmpty() && diags.getProjectAlerts().isEmpty()) {
            sb.append("  - No alerts found.\n");
        } else {
            // 1. Project Problems (High-level)
            if (!diags.getProjectAlerts().isEmpty()) {
                sb.append("  ### Project Problems\n");
                for (ProjectAlert alert : diags.getProjectAlerts()) {
                    sb.append("    - [").append(alert.getSeverity()).append("] ")
                      .append(alert.getDisplayName()).append(": ").append(alert.getDescription().replace("\n", " ")).append("\n");
                }
            }

            // 2. Java Compiler Alerts (File-level)
            if (!diags.getJavacAlerts().isEmpty()) {
                sb.append("  ### Java Compiler Alerts\n");
                for (JavacAlert alert : diags.getJavacAlerts()) {
                    sb.append("    - [").append(alert.getKind()).append("] ")
                      .append(alert.getFilePath()).append(":").append(alert.getLineNumber())
                      .append(" - ").append(alert.getMessage().replace("\n", " ")).append("\n");
                }
            }
        }
        
        ragMessage.addTextPart(sb.toString());
    }

    /**
     * Toggles providing status and triggers a UI refresh.
     * <p>
     * Implementation details:
     * Notifies the IDE that the project icon should be redrawn to reflect 
     * the new context state.
     * </p>
     * 
     * @param enabled New state.
     */
    @Override
    public void setProviding(boolean enabled) {
        boolean old = isProviding();
        super.setProviding(enabled);
        if (old != enabled && parent instanceof ProjectContextProvider pcp) {
            FilesContextActionLogic.fireRefreshRecursive(pcp.getProject().getProjectDirectory());
        }
    }
}
