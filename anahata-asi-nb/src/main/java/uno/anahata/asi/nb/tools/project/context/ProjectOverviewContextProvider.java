/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.context;

import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.nb.tools.maven.DeclaredArtifact;
import uno.anahata.asi.nb.tools.maven.DependencyGroup;
import uno.anahata.asi.nb.tools.maven.DependencyScope;
import uno.anahata.asi.nb.tools.project.ProjectOverview;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.nb.tools.files.nb.FilesContextActionLogic;

/**
 * Provides a compact, real-time overview of a project's metadata and dependencies.
 * This provider injects a Markdown-formatted summary of the project into the
 * AI's prompt augmentation (RAG) message.
 * 
 * @author anahata
 */
@Slf4j
public class ProjectOverviewContextProvider extends BasicContextProvider {

    /** The parent Projects toolkit. */
    private final Projects projectsToolkit;
    
    /** The absolute path to the project directory. */
    private final String projectPath;

    /**
     * Constructs a new overview provider for a specific project.
     * <p>
     * Implementation details:
     * Stores references to the parent toolkit and target path.
     * </p>
     * 
     * @param projectsToolkit The parent Projects toolkit.
     * @param projectPath The absolute path to the project.
     */
    public ProjectOverviewContextProvider(Projects projectsToolkit, String projectPath) {
        super("overview", "Project Overview", "Metadata, actions, and dependencies");
        this.projectsToolkit = projectsToolkit;
        this.projectPath = projectPath;
    }

    /**
     * Injects the project's Markdown overview into the RAG message.
     * <p>
     * Implementation details:
     * Fetches the project metadata from the toolkit and generates a structured 
     * Markdown summary including packaging, versions, actions, and dependencies.
     * </p>
     * 
     * @param ragMessage The target RAG message.
     * @throws Exception if project overview cannot be fetched.
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        ProjectOverview overview = projectsToolkit.getOverview(projectPath);
        ragMessage.addTextPart(generateMarkdown(overview));
    }

    /**
     * Toggles providing status and triggers a UI refresh.
     * <p>
     * Implementation details:
     * Updates base state and notifies the IDE to redraw project icons.
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

    /**
     * Generates a Markdown string representing the project overview.
     * <p>
     * Implementation details:
     * Builds a comprehensive Markdown report including project identity, path, 
     * packaging, Java versions, encoding, CoS status, actions, and dependencies.
     * </p>
     * 
     * @param overview The project overview data.
     * @return A Markdown-formatted string.
     */
    private String generateMarkdown(ProjectOverview overview) {
        StringBuilder sb = new StringBuilder();
        
        sb.append("\n# Project: ").append(overview.getDisplayName()).append(" (`").append(overview.getId()).append("`)\n");
        sb.append("  - Path: `").append(overview.getProjectDirectory()).append("`\n");
        if (overview.getPackaging() != null) {
            sb.append("  - Packaging: `").append(overview.getPackaging()).append("`\n");
        }
        sb.append("  - Java Version: ").append(overview.getJavaSourceLevel()).append(" (source), ").append(overview.getJavaTargetLevel()).append(" (target)\n");
        sb.append("  - Encoding: ").append(overview.getSourceEncoding()).append("\n");
        sb.append("  - Compile on Save: ").append(overview.getCompileOnSave()).append("\n");
        sb.append("  - Actions: `").append(String.join("`, `", overview.getActions())).append("`\n");

        if (overview.getMavenDeclaredDependencies() != null && !overview.getMavenDeclaredDependencies().isEmpty()) {
            sb.append("\n  ## Declared Maven Dependencies\n");
            for (DependencyScope scope : overview.getMavenDeclaredDependencies()) {
                sb.append(formatDependencyScope(scope, "    "));
            }
        }

        return sb.toString();
    }

    /**
     * Formats a Maven dependency scope and its artifacts into Markdown.
     * <p>
     * Implementation details:
     * Iterates through groups and artifacts for the given scope, applying 
     * consistent indentation and formatting.
     * </p>
     * 
     * @param scope The dependency scope to format.
     * @param indent The current indentation string.
     * @return A Markdown-formatted string for the scope.
     */
    private String formatDependencyScope(DependencyScope scope, String indent) {
        StringBuilder sb = new StringBuilder();
        sb.append(indent).append("- Scope: `").append(scope.getScope()).append("`\n");
        String childIndent = indent + "  ";
        for (DependencyGroup group : scope.getGroups()) {
            String artifacts = group.getArtifacts().stream()
                    .map(DeclaredArtifact::getId)
                    .collect(Collectors.joining(", "));
            sb.append(childIndent).append("- `").append(group.getId()).append("`: ").append(artifacts).append("\n");
        }
        return sb.toString();
    }
}
