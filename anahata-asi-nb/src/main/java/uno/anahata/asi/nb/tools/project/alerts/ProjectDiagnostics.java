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
    /**
     * The display name of the project associated with these diagnostics.
     */
    @Schema(description = "The display name of the project.")
    private final String projectName;
    /**
     * A list of Java compiler alerts found during the project scan.
     */
    @Schema(description = "A list of Java compiler alerts.")
    private final List<JavacAlert> javacAlerts = new ArrayList<>();
    /**
     * A list of high-level project problems found during the project scan.
     */
    @Schema(description = "A list of high-level project problems.")
    private final List<ProjectAlert> projectAlerts = new ArrayList<>();

    /**
     * Constructs a new diagnostics container for the specified project.
     * @param projectName The display name of the project.
     */
    public ProjectDiagnostics(String projectName) {
        this.projectName = projectName;
    }

    /**
     * Adds a Java compiler alert to the diagnostics.
     * @param alert The compiler alert to add.
     */
    public void addJavacAlert(JavacAlert alert) {
        this.javacAlerts.add(alert);
    }

    /**
     * Adds a high-level project alert to the diagnostics.
     * @param alert The project alert to add.
     */
    public void addProjectAlert(ProjectAlert alert) {
        this.projectAlerts.add(alert);
    }
}
