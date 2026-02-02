/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.context;

import java.io.File;
import lombok.extern.slf4j.Slf4j;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.nodes.Node;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.nb.tools.project.ProjectFile;
import uno.anahata.asi.nb.tools.project.ProjectFiles;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.nb.tools.project.SourceFolder;

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
     * {@inheritDoc}
     * Populates the RAG message with a Markdown tree of the project's files.
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        ProjectFiles files = projectsToolkit.getProjectFiles(projectPath);
        ragMessage.addTextPart(generateMarkdown(files));
    }

    /**
     * Generates a Markdown string representing the project file tree.
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
        for (SourceFolder sourceFolder : files.getSourceFolders()) {
            sb.append(formatSourceFolder(sourceFolder, "    ", sourceFolder.getPath()));
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
        String folderName = folder.getDisplayName() != null ? folder.getDisplayName() : new File(folder.getPath()).getName();
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
        
        if (file.getContextStatus() != null) {
            statusBuilder.append(" ").append(file.getContextStatus());
        }
        
        try {
            FileObject fo = FileUtil.toFileObject(new File(file.getPath()));
            if (fo != null) {
                DataObject dobj = DataObject.find(fo);
                Node node = dobj.getNodeDelegate();
                
                String annotated = node.getHtmlDisplayName();
                if (annotated == null) {
                    annotated = node.getDisplayName();
                }
                
                if (annotated != null && !annotated.equals(fo.getNameExt())) {
                    String extra = annotated.replace(fo.getNameExt(), "").trim();
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
}
