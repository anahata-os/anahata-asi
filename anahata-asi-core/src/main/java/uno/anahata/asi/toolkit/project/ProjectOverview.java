/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.project;

import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import uno.anahata.asi.toolkit.maven.DependencyScope;

/**
 * Represents a high-level, structured overview of an open project or submodule.
 * <p>
 * This DTO aggregates critical project and module metadata including packaging type,
 * Java versions (source and target levels), source encoding, supported IDE actions,
 * token-efficient declared Maven dependencies (via {@link DependencyScope}),
 * 'Compile on Save' status, and active VCS/Git overview information.
 * It serves as the primary "identity card" for projects and modules in the ASI prompt.
 * </p>
 * 
 * @author anahata
 */
@Schema(description = "Represents a high-level, structured overview of a project or module, including its metadata, supported actions, and declared dependencies.")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public final class ProjectOverview implements Serializable {

    /** The project or module ID, which is typically the folder name or artifactId. */
    @Schema(description = "The project or module ID, which is typically the folder name or artifactId.", example = "anahata-asi-intellij")
    private String id;

    /** The human-readable display name of the project or module as shown in the IDE. */
    @Schema(description = "The human-readable display name of the project or module.", example = "Anahata ASI IntelliJ")
    private String displayName;

    /** The HTML-formatted display name, potentially containing IDE status annotations (e.g. Git branch). */
    @Schema(description = "The HTML-formatted display name, containing IDE annotations.")
    private String htmlDisplayName;

    /** The absolute physical path to the project or module's root directory. */
    @Schema(description = "The absolute path to the project or module root directory.", example = "/home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-intellij")
    private String projectDirectory;
    
    /** The packaging type as defined in pom.xml (e.g., 'jar', 'pom', 'nbm'). Null for non-Maven projects. */
    @Schema(description = "The packaging type as defined in the pom.xml (e.g., 'jar', 'pom', 'nbm'). This is null for non-Maven projects.", example = "jar")
    private String packaging;

    /** A list of supported IDE Project Actions that can be invoked (e.g., 'build', 'run'). */
    @Schema(description = "A list of supported high-level Project Actions that can be invoked on the Project (e.g., 'build', 'run').")
    private List<String> actions;
    
    /** The list of dependencies directly declared in the pom.xml, grouped by scope and groupId for maximum token efficiency. */
    @Schema(description = "The list of dependencies directly declared in the pom.xml, grouped by scope and groupId for maximum token efficiency.")
    private List<DependencyScope> mavenDeclaredDependencies;
    
    /** The Java source level version of the project (e.g., '1.8', '11', '17', '21', '25'). */
    @Schema(description = "The Java source level version of the project (e.g., '1.8', '11', '17', '21', '25').", example = "25")
    private String javaSourceLevel;
    
    /** The Java target level version for the compiled bytecode (e.g., '1.8', '11', '17', '21', '25'). */
    @Schema(description = "The Java target level version for the compiled bytecode (e.g., '1.8', '11', '17', '21', '25').", example = "25")
    private String javaTargetLevel;
    
    /** The source file encoding for the project (e.g., 'UTF-8'). */
    @Schema(description = "The source file encoding for the project (e.g., 'UTF-8').", example = "UTF-8")
    private String sourceEncoding;

    /** The effective status of 'Compile on Save' (includes the configuration source). */
    @Schema(description = "The status of 'Compile on Save' for this project (e.g., 'all', 'none', 'Enabled', 'Disabled').", example = "all (IDE Override)")
    private String compileOnSave;

    /** Formatted Git or VCS overview if this project/module is the repository root, or null if not applicable. */
    @Schema(description = "Formatted Git or VCS overview if this project/module is the repository root, or null if not applicable.")
    private String vcsOverview;

    /**
     * Renders this project or module overview into clean, standardized Markdown for the AI prompt.
     *
     * @return Formatted Markdown text.
     */
    public String toMarkdown() {
        StringBuilder sb = new StringBuilder();
        sb.append("\n# Project: ").append(displayName != null ? displayName : id);
        if (id != null && !id.equals(displayName)) {
            sb.append(" (`").append(id).append("`)");
        }
        sb.append("\n");
        if (projectDirectory != null) {
            sb.append("  - Path: `").append(projectDirectory).append("`\n");
        }
        if (packaging != null && !packaging.isBlank()) {
            sb.append("  - Packaging: `").append(packaging).append("`\n");
        }
        if (javaSourceLevel != null || javaTargetLevel != null) {
            sb.append("  - Java Version: ").append(javaSourceLevel != null ? javaSourceLevel : "unknown").append(" (source), ")
              .append(javaTargetLevel != null ? javaTargetLevel : "unknown").append(" (target)\n");
        }
        if (sourceEncoding != null && !sourceEncoding.isBlank()) {
            sb.append("  - Encoding: ").append(sourceEncoding).append("\n");
        }
        if (compileOnSave != null && !compileOnSave.isBlank()) {
            sb.append("  - Compile on Save: ").append(compileOnSave).append("\n");
        }
        if (actions != null && !actions.isEmpty()) {
            sb.append("  - Actions: `").append(String.join("`, `", actions)).append("`\n");
        }

        if (mavenDeclaredDependencies != null && !mavenDeclaredDependencies.isEmpty()) {
            sb.append("\n  ## Declared Maven Dependencies\n");
            for (DependencyScope scope : mavenDeclaredDependencies) {
                sb.append("    - Scope: `").append(scope.getScope()).append("`\n");
                if (scope.getGroups() != null) {
                    for (uno.anahata.asi.toolkit.maven.DependencyGroup group : scope.getGroups()) {
                        String artifacts = group.getArtifacts() != null
                                ? group.getArtifacts().stream()
                                        .map(uno.anahata.asi.toolkit.maven.DeclaredArtifact::getId)
                                        .collect(Collectors.joining(", "))
                                : "";
                        sb.append("      - `").append(group.getId()).append("`: ").append(artifacts).append("\n");
                    }
                }
            }
        }

        if (vcsOverview != null && !vcsOverview.isBlank()) {
            sb.append("\n  ").append(vcsOverview).append("\n");
        }

        return sb.toString();
    }
}
