/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.alerts;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.List;
import lombok.Data;

/**
 * A container for all diagnostics found during a project scan.
 */
@Schema(description = "A container for all diagnostics found during a project scan, including Java compiler alerts and high-level project problems.")
@Data
public final class ProjectDiagnostics {
    @Schema(description = "The display name of the project.")
    private final String projectName;
    @Schema(description = "A list of Java compiler alerts.")
    private final List<JavacAlert> javacAlerts = new ArrayList<>();
    @Schema(description = "A list of high-level project problems.")
    private final List<ProjectAlert> projectAlerts = new ArrayList<>();

    public ProjectDiagnostics(String projectName) {
        this.projectName = projectName;
    }

    public void addJavacAlert(JavacAlert alert) {
        this.javacAlerts.add(alert);
    }

    public void addProjectAlert(ProjectAlert alert) {
        this.projectAlerts.add(alert);
    }
}
