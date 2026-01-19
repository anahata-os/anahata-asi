package uno.anahata.asi.nb.tools.project;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

/**
 *
 * @author anahata
 */
@Schema(description = "An open project in netbeans")
@NoArgsConstructor
@AllArgsConstructor
public class OpenProject {
    @Schema(description = "absolute path to the project's folder")
    private String path;
    private boolean includeAlerts;
    private boolean includeOverview;
    private ProjectOverview overview;
    private String alerts;
}
