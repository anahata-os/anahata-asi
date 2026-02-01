/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.context;

import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.project.Project;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.nodes.Node;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.TextPart;
import uno.anahata.asi.nb.tools.maven.DeclaredArtifact;
import uno.anahata.asi.nb.tools.maven.DependencyGroup;
import uno.anahata.asi.nb.tools.maven.DependencyScope;
import uno.anahata.asi.nb.tools.project.ProjectFile;
import uno.anahata.asi.nb.tools.project.ProjectOverview;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.nb.tools.project.SourceFolder;
import uno.anahata.asi.nb.tools.project.nb.AnahataProjectAnnotator;

/**
 * Provides a compact, real-time overview of a project's structure and metadata.
 * This provider injects a Markdown-formatted summary of the project into the
 * AI's prompt augmentation (RAG) message.
 * <p>
 * It also attempts to extract IDE-level annotations (like Git status) for each file
 * by querying the Node delegate.
 * </p>
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
        super("overview", "Project Overview", "Source tree, dependencies, and metadata");
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
            AnahataProjectAnnotator.fireRefreshAll(pcp.getProject());
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
        sb.append("  - Actions: `").append(String.join("`, `", overview.getActions())).append("`\n");

        sb.append("\n  ## Root Directory\n");
        sb.append("    - Folders: `").append(String.join("`, `", overview.getRootFolderNames())).append("`\n");
        for (ProjectFile file : overview.getRootFiles()) {
            sb.append(formatProjectFile(file, "    "));
        }

        sb.append("\n  ## Source Folders\n");
        for (SourceFolder sourceFolder : overview.getSourceFolders()) {
            sb.append(formatSourceFolder(sourceFolder, "    ", sourceFolder.getPath()));
        }

        if (overview.getMavenDeclaredDependencies() != null && !overview.getMavenDeclaredDependencies().isEmpty()) {
            sb.append("\n  ## Declared Maven Dependencies\n");
            for (DependencyScope scope : overview.getMavenDeclaredDependencies()) {
                sb.append(formatDependencyScope(scope, "    "));
            }
        }

        return sb.toString();
    }

    /**
     * Recursively formats a source folder and its contents into Markdown.
     * 
     * @param folder The source folder to format.
     * @param indent The current indentation string.
     * @param basePath The base path of the project.
     * @return A Markdown-formatted string for the folder.
     */
    private String formatSourceFolder(SourceFolder folder, String indent, String basePath) {
        StringBuilder sb = new StringBuilder();
        String folderName = folder.getDisplayName() != null ? folder.getDisplayName() : new java.io.File(folder.getPath()).getName();
        sb.append(indent).append("- 📂 ").append(folderName).append("\n");

        String childIndent = indent + "  ";
        if (folder.getFiles() != null) {
            for (ProjectFile file : folder.getFiles()) {
                sb.append(formatProjectFile(file, childIndent));
            }
        }
        if (folder.getSubfolders() != null) {
            for (SourceFolder subfolder : folder.getSubfolders()) {
                sb.append(formatSourceFolder(subfolder, childIndent, basePath));
            }
        }
        return sb.toString();
    }

    /**
     * Formats a project file into a Markdown list item, including its context status
     * and any IDE-level annotations (like Git status).
     * 
     * @param file The project file to format.
     * @param indent The current indentation string.
     * @return A Markdown-formatted string for the file.
     */
    private String formatProjectFile(ProjectFile file, String indent) {
        StringBuilder statusBuilder = new StringBuilder();
        
        // 1. Add AI Context status
        if (file.getContextStatus() != null) {
            statusBuilder.append(" ").append(file.getContextStatus());
        }
        
        // 2. Attempt to extract IDE annotations (e.g. Git status) via Node delegate
        try {
            FileObject fo = FileUtil.toFileObject(new java.io.File(file.getPath()));
            if (fo != null) {
                DataObject dobj = DataObject.find(fo);
                Node node = dobj.getNodeDelegate();
                
                // Prefer HTML display name as it contains branch info and colors
                String annotated = node.getHtmlDisplayName();
                if (annotated == null) {
                    annotated = node.getDisplayName();
                }
                
                if (annotated != null && !annotated.equals(fo.getNameExt())) {
                    // Extract the annotation part (usually in brackets or colored)
                    String extra = annotated.replace(fo.getNameExt(), "").trim();
                    // Clean up HTML tags if present
                    extra = extra.replaceAll("<[^>]*>", "");
                    if (!extra.isEmpty()) {
                        statusBuilder.append(" ").append(extra);
                    }
                }
            }
        } catch (Exception e) {
            log.warn("Failed to extract IDE annotations for: {}", file.getPath(), e);
        }
        
        return String.format("%s- 📄 %s%s\n", indent, file.getName(), statusBuilder.toString());
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
