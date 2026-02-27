/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.project.context;

import java.io.File;
import lombok.extern.slf4j.Slf4j;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.nb.tools.project.ProjectFile;
import uno.anahata.asi.nb.tools.project.ProjectFiles;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.nb.tools.project.SourceFolder;
import uno.anahata.asi.nb.tools.files.nb.FilesContextActionLogic;

/**
 * Provides a real-time view of a project's file and folder structure.
 * This provider injects a Markdown-formatted tree of the project into the
 * AI's prompt augmentation (RAG) message.
 * 
 * @author anahata
 */
@Slf4j
public class ProjectFilesContextProvider extends BasicContextProvider {

    /** The parent Projects toolkit. */
    private final Projects projectsToolkit;
    
    /** The absolute path to the project directory. */
    private final String projectPath;

    /**
     * Constructs a new files provider for a specific project.
     * 
     * @param projectsToolkit The parent Projects toolkit.
     * @param projectPath The absolute path to the project.
     */
    public ProjectFilesContextProvider(Projects projectsToolkit, String projectPath) {
        super("files", "Project Files", "Source tree and root directory contents");
        this.projectsToolkit = projectsToolkit;
        this.projectPath = projectPath;
    }

    /**
     * Injects the project's file tree into the RAG message.
     * <p>
     * Implementation details:
     * Fetches the current file structural data from the Projects toolkit and 
     * generates a Markdown tree. It distinguishes between root files and 
     * source-group folders for clarity.
     * </p>
     * 
     * @param ragMessage The target RAG message.
     * @throws Exception if project files cannot be retrieved.
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        ProjectFiles files = projectsToolkit.getProjectFiles(projectPath);
        ragMessage.addTextPart(generateMarkdown(files));
    }

    /**
     * Toggles providing status and triggers a UI refresh.
     * <p>
     * Implementation details:
     * Notifies the IDE that the project icon should be redrawn to reflect 
     * the new context state.
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
     * Generates a Markdown string representing the project file tree.
     * <p>
     * Implementation details:
     * This method splits the output into the Root Directory (flat list) and 
     * Source Folders (hierarchical tree). It resolves the relative path for
     * each top-level source folder to provide context for the AI.
     * </p>
     * 
     * @param files The project files data.
     * @return A Markdown-formatted string.
     */
    private String generateMarkdown(ProjectFiles files) {
        StringBuilder sb = new StringBuilder();
        sb.append("\n  ## Root Directory\n");
        sb.append("    - Folders: `").append(String.join("`, `", files.getRootFolderNames())).append("`\n");
        for (ProjectFile file : files.getRootFiles()) {
            sb.append(formatProjectFile(file, "    "));
        }

        sb.append("\n  ## Source Folders\n");
        FileObject projectFo = FileUtil.toFileObject(new File(projectPath));
        for (SourceFolder sourceFolder : files.getSourceFolders()) {
            String relPath = null;
            if (projectFo != null) {
                FileObject folderFo = FileUtil.toFileObject(new File(sourceFolder.getPath()));
                if (folderFo != null) {
                    // Calculate path relative to project root for top-level source groups
                    relPath = FileUtil.getRelativePath(projectFo, folderFo);
                }
            }
            sb.append(formatSourceFolder(sourceFolder, "    ", sourceFolder.getPath(), relPath));
        }

        return sb.toString();
    }

    /**
     * Recursively formats a source folder and its contents into a Markdown tree.
     * <p>
     * Implementation details:
     * Displays the folder name (or display name) followed by an optional 
     * relative path in parentheses for top-level groups.
     * </p>
     * 
     * @param folder The source folder to format.
     * @param indent The current indentation level.
     * @param basePath The base path of the project.
     * @param relPath Optional relative path for display next to the folder.
     * @return A Markdown-formatted string for the folder.
     */
    private String formatSourceFolder(SourceFolder folder, String indent, String basePath, String relPath) {
        StringBuilder sb = new StringBuilder();
        String folderName = folder.getDisplayName() != null ? folder.getDisplayName() : new File(folder.getPath()).getName();
        sb.append(indent).append("- 📂 ").append(folderName);
        if (relPath != null && !relPath.isEmpty()) {
            sb.append(" (").append(relPath).append(")");
        }
        sb.append("\n");

        String childIndent = indent + "  ";
        if (folder.getFiles() != null) {
            for (ProjectFile file : folder.getFiles()) {
                sb.append(formatProjectFile(file, childIndent));
            }
        }
        if (folder.getSubfolders() != null) {
            for (SourceFolder subfolder : folder.getSubfolders()) {
                // We only show relative paths for top-level folders to avoid clutter
                sb.append(formatSourceFolder(subfolder, childIndent, basePath, null));
            }
        }
        return sb.toString();
    }

    /**
     * Formats a project file into a Markdown list item.
     * <p>
     * Implementation details:
     * Includes the file name and any IDE-level annotations (e.g., Git status flags)
     * extracted from the annotated name.
     * </p>
     * 
     * @param file The project file to format.
     * @param indent The current indentation string.
     * @return A Markdown-formatted string for the file.
     */
    private String formatProjectFile(ProjectFile file, String indent) {
        StringBuilder statusBuilder = new StringBuilder();
        
        String annotated = file.getAnnotatedName();
        if (annotated != null && !annotated.equals(file.getName())) {
            String extra = annotated.replace(file.getName(), "").trim();
            if (!extra.isEmpty()) {
                statusBuilder.append(" ").append(extra);
            }
        }
        
        return String.format("%s- 📄 %s%s\n", indent, file.getName(), statusBuilder.toString());
    }
}
