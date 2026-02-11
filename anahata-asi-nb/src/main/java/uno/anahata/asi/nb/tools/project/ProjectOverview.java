/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import uno.anahata.asi.nb.tools.maven.DependencyScope;

/**
 * Represents a high-level, structured overview of a project, including its metadata
 * and declared dependencies.
 * This is a Java 8-compatible, immutable data class.
 *
 * @author Anahata
 */
@Schema(description = "Represents a high-level, structured overview of a project, including its metadata, supported actions, and declared dependencies.")
@Data
@AllArgsConstructor
public final class ProjectOverview {

    @Schema(description = "The project ID, which is typically the folder name.")
    private final String id;

    @Schema(description = "The human-readable display name of the project.")
    private final String displayName;

    @Schema(description = "The HTML-formatted display name, containing IDE annotations.")
    private final String htmlDisplayName;

    @Schema(description = "The absolute path to the project's root directory.")
    private final String projectDirectory;
    
    @Schema(description = "The project's packaging type as defined in the pom.xml (e.g., 'jar', 'nbm', 'nbm-application'). This is null for non-Maven projects.")
    private final String packaging;

    @Schema(description = "A list of supported high-level NetBeans Project Actions that can be invoked on the Project (e.g., 'build', 'run').")
    private final List<String> actions;
    
    @Schema(description = "The list of dependencies directly declared in the pom.xml, grouped by scope and groupId for maximum token efficiency. (by MavenPom.getDeclaredDependencies)")
    private final List<DependencyScope> mavenDeclaredDependencies;
    
    @Schema(description = "The Java source level version of the project (e.g., '1.8', '11', '17'), obtained in a project-agnostic way.")
    private final String javaSourceLevel;
    
    @Schema(description = "The Java target level version for the compiled bytecode (e.g., '1.8', '11', '17').")
    private final String javaTargetLevel;
    
    @Schema(description = "The source file encoding for the project (e.g., 'UTF-8').")
    private final String sourceEncoding;

    @Schema(description = "Whether 'Compile on Save' is enabled for this project.")
    private final boolean compileOnSave;
    
}
