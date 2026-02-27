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
     * {@inheritDoc}
     * Populates the RAG message with a Markdown overview of the project.
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        ProjectOverview overview = projectsToolkit.getOverview(projectPath);
        ragMessage.addTextPart(generateMarkdown(overview));
    }

    /**
     * {@inheritDoc}
     * Overridden to trigger a project UI refresh when the overview is toggled.
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
