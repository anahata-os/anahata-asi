/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.project;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import uno.anahata.asi.toolkit.maven.DependencyScope;


/**
 * Represents a high-level, structured overview of a NetBeans project.
 * <p>
 * This DTO aggregates critical project metadata including environmental settings 
 * (Java versions, encoding), runtime capabilities (actions), and a token-efficient 
 * representation of declared dependencies. It serves as the primary "identity card" 
 * for a project within the ASI framework.
 * </p>
 * 
 * @author anahata
 */
@Schema(description = "Represents a high-level, structured overview of a project, including its metadata, supported actions, and declared dependencies.")
@Data
@AllArgsConstructor
public final class ProjectOverview {

    /** The project ID, which is typically the folder name. */
    @Schema(description = "The project ID, which is typically the folder name.", example = "anahata-asi-nb")
    private final String id;

    /** The human-readable display name of the project as shown in the IDE. */
    @Schema(description = "The human-readable display name of the project.", example = "Anahata ASI NetBeans")
    private final String displayName;

    /** The HTML-formatted display name, potentially containing IDE status annotations (e.g. Git branch). */
    @Schema(description = "The HTML-formatted display name, containing IDE annotations.")
    private final String htmlDisplayName;

    /** The absolute physical path to the project's root directory. */
    @Schema(description = "The absolute path to the project's root directory.", example = "/home/pablo/NetBeansProjects/anahata-asi-nb")
    private final String projectDirectory;
    
    /** The project's packaging type (e.g., 'jar', 'nbm'). Null for non-Maven projects. */
    @Schema(description = "The project's packaging type as defined in the pom.xml (e.g., 'jar', 'nbm', 'nbm-application'). This is null for non-Maven projects.", example = "nbm")
    private final String packaging;

    /** A list of supported NetBeans Project Actions that can be invoked (e.g., 'build', 'run'). */
    @Schema(description = "A list of supported high-level NetBeans Project Actions that can be invoked on the Project (e.g., 'build', 'run').")
    private final List<String> actions;
    
    /** The list of dependencies directly declared in the pom.xml, optimized for token efficiency. */
    @Schema(description = "The list of dependencies directly declared in the pom.xml, grouped by scope and groupId for maximum token efficiency.")
    private final List<DependencyScope> mavenDeclaredDependencies;
    
    /** The Java source level version of the project (e.g., '17'). */
    @Schema(description = "The Java source level version of the project (e.g., '1.8', '11', '17'), obtained in a project-agnostic way.", example = "17")
    private final String javaSourceLevel;
    
    /** The Java target level version for the compiled bytecode. */
    @Schema(description = "The Java target level version for the compiled bytecode (e.g., '1.8', '11', '17').", example = "17")
    private final String javaTargetLevel;
    
    /** The source file encoding for the project. */
    @Schema(description = "The source file encoding for the project (e.g., 'UTF-8').", example = "UTF-8")
    private final String sourceEncoding;

    /** The effective status of 'Compile on Save' (includes the configuration source). */
    @Schema(description = "The status of 'Compile on Save' for this project (e.g., 'all', 'none', 'Enabled', 'Disabled').", example = "all (IDE Override)")
    private final String compileOnSave;

    /** Formatted Git or VCS overview if this project is the repository root, or null if not applicable. */
    @Schema(description = "Formatted Git or VCS overview if this project is the repository root, or null if not applicable.")
    private final String vcsOverview;
}
