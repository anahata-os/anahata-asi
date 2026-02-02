/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.project.alerts;

import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.nb.tools.project.Projects;

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
        // Disabled by default to avoid performance impact on large projects
        setProviding(false);
    }

    /**
     * {@inheritDoc}
     * Populates the RAG message with a Markdown representation of the project diagnostics.
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
}
