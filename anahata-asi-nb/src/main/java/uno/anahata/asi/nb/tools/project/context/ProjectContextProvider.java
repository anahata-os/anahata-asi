/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.context;

import java.io.File;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.openide.filesystems.FileObject;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.context.ContextPosition;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.resource.AbstractPathResource;
import uno.anahata.asi.model.resource.files.TextFileResource;
import uno.anahata.asi.nb.tools.files.nb.NbFiles;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.nb.tools.project.ProjectOverview;
import uno.anahata.asi.nb.tools.maven.DependencyScope;
import uno.anahata.asi.nb.tools.maven.DependencyGroup;
import uno.anahata.asi.nb.tools.maven.DeclaredArtifact;
import uno.anahata.asi.nb.tools.project.alerts.ProjectAlertsContextProvider;
import uno.anahata.asi.nb.tools.files.nb.FilesContextActionLogic;

/**
 * A hierarchical context provider for a specific NetBeans project.
 * Consolidates metadata, actions, dependencies, and project-specific instructions (anahata.md).
 * 
 * @author anahata
 */
@Slf4j
public class ProjectContextProvider extends BasicContextProvider {

    /** The parent Projects toolkit. */
    private final Projects projectsToolkit;

    /** The NetBeans project instance. Transient to support serialization. */
    private transient Project project;

    /** The absolute path to the project directory. */
    @Getter
    private final String projectPath;
    
    /**
     * Constructs a new project context provider.
     * <p>
     * Implementation details:
     * Initializes project-specific child providers (Files, Components, Alerts) 
     * and triggers initial anahata.md synchronization.
     * </p>
     * 
     * @param projectsToolkit The parent Projects toolkit.
     * @param project The NetBeans project instance.
     */
    public ProjectContextProvider(Projects projectsToolkit, Project project) {
        super(Projects.getCanonicalPath(project.getProjectDirectory()), 
              "Overview", 
              "Context for project: " + ProjectUtils.getInformation(project).getDisplayName());
        this.projectsToolkit = projectsToolkit;
        this.project = project;
        this.projectPath = Projects.getCanonicalPath(project.getProjectDirectory());
        
        // Register with parent
        this.setParent(projectsToolkit);
        
        // Initialize structural children
        ProjectFilesContextProvider files = new ProjectFilesContextProvider(projectsToolkit, projectPath);
        files.setParent(this);
        children.add(files);

        ProjectComponentsContextProvider components = new ProjectComponentsContextProvider(projectsToolkit, projectPath);
        components.setParent(this);
        children.add(components);

        ProjectAlertsContextProvider alerts = new ProjectAlertsContextProvider(projectsToolkit, projectPath);
        alerts.setParent(this);
        children.add(alerts);

        // Sync anahata.md on creation
        syncMdResource();
    }

    /**
     * Gets the NetBeans project instance, restoring it from the path if necessary.
     * @return The project instance, or null if it cannot be found or is closed.
     */
    public Project getProject() {
        if (project == null) {
            try {
                project = Projects.findOpenProject(projectPath);
            } catch (Exception e) {
                log.debug("Project not open at path: {}", projectPath);
            }
        }
        return project;
    }

    /**
     * Returns the dynamic display name of the project.
     * @return The project's display name.
     */
    @Override
    public String getName() {        
        return "Overview";
    }

    /**
     * Toggles the providing state and synchronizes the anahata.md resource.
     * @param enabled The new state.
     */
    @Override
    public void setProviding(boolean enabled) {
        boolean old = isProviding();
        super.setProviding(enabled);
        
        syncMdResource();
        
        if (old != enabled) {
            Project p = getProject();
            if (p != null) {
                FilesContextActionLogic.fireRefreshRecursive(p.getProjectDirectory());
            }
        }
    }

    /**
     * Injects the project's Markdown overview into the RAG message.
     * @param ragMessage The target RAG message.
     * @throws Exception if fetching overview fails.
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        ProjectOverview overview = projectsToolkit.getOverview(projectPath);
        ragMessage.addTextPart(generateMarkdown(overview));
    }

    /**
     * Synchronizes the anahata.md file with the agi's resource manager.
     * Uses path-based lookup to honor manual user unloads.
     */
    private void syncMdResource() {
        String mdPath = new File(projectPath, "anahata.md").getAbsolutePath();
        Optional<? extends AbstractPathResource<?>> existing = projectsToolkit.getResourceManager().findByPath(mdPath);

        if (isProviding()) {
            if (existing.isEmpty()) {
                try {
                    Project p = getProject();
                    if (p == null) return;
                    
                    FileObject mdFo = Projects.ensureAnahataMdExists(p);
                    NbFiles filesToolkit = projectsToolkit.getToolkit(NbFiles.class);
                    TextFileResource resource = filesToolkit.loadTextFileInternal(mdFo.getPath(), null);
                    
                    // Force the position to SYSTEM_INSTRUCTIONS
                    resource.setContextPosition(ContextPosition.SYSTEM_INSTRUCTIONS);
                    log.info("Registered anahata.md as SYSTEM_INSTRUCTIONS for project: {}", projectPath);
                } catch (Exception e) {
                    log.error("Failed to sync anahata.md for project: " + projectPath, e);
                }
            } else if (existing.get() instanceof TextFileResource tfr) {
                // Ensure it's in the right position if it exists
                tfr.setContextPosition(ContextPosition.SYSTEM_INSTRUCTIONS);
            }
        } else {
            existing.ifPresent(resource -> {
                projectsToolkit.getResourceManager().unregister(resource.getId());
                log.info("Unregistered anahata.md for project: {}", projectPath);
            });
        }
    }

    /**
     * Generates a Markdown string representing the project overview.
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
