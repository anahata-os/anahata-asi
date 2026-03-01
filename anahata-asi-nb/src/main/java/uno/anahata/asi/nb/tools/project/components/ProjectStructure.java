/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.components;

import java.util.ArrayList;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.openide.filesystems.FileObject;

/**
 * The high-level orchestrator for the refined project structure model.
 * <p>
 * This class handles the initialization and recursive construction of 
 * the project map from a NetBeans Project instance. It categorizes 
 * elements into root files, root folders, and specialized source groups 
 * (Java and Resources) to provide a complete architectural overview.
 * </p>
 * 
 * @author Anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public final class ProjectStructure extends ProjectNode {

    /** 
     * The display name of the project. 
     */
    private String projectName;

    /** 
     * Files located directly in the project root directory. 
     */
    @Builder.Default
    private List<ProjectComponent> rootFiles = new ArrayList<>();

    /** 
     * Names of folders located in the project root that are not source groups. 
     */
    @Builder.Default
    private List<String> rootFolders = new ArrayList<>();

    /** 
     * Containers for logical Java source roots. 
     */
    @Builder.Default
    private List<JavaSourceGroup> javaSourceGroups = new ArrayList<>();

    /** 
     * Containers for physical resource source roots. 
     */
    @Builder.Default
    private List<ResourceSourceGroup> resourceSourceGroups = new ArrayList<>();

    /**
     * Builds the complete project structure recursively.
     * <p>
     * Implementation details:
     * 1. Identifies the project's root directory and generic source groups.
     * 2. Classifies root-level items as files or folders.
     * 3. Specialized builders are invoked for each Java and Resource source group.
     * </p>
     * 
     * @param project The NetBeans project instance to map.
     * @throws Exception if construction of any constituent group fails.
     */
    public ProjectStructure(Project project) throws Exception {
        this.projectName = ProjectUtils.getInformation(project).getDisplayName();
        this.rootFiles = new ArrayList<>();
        this.rootFolders = new ArrayList<>();
        this.javaSourceGroups = new ArrayList<>();
        this.resourceSourceGroups = new ArrayList<>();
        
        FileObject root = project.getProjectDirectory();
        Sources sources = ProjectUtils.getSources(project);
        List<FileObject> sgRoots = new ArrayList<>();
        for (SourceGroup sg : sources.getSourceGroups(Sources.TYPE_GENERIC)) {
            sgRoots.add(sg.getRootFolder());
        }

        for (FileObject child : root.getChildren()) {
            if (child.isFolder()) {
                if (!sgRoots.contains(child)) {
                    rootFolders.add(child.getNameExt());
                }
            } else {
                rootFiles.add(new ProjectComponent(child, null));
            }
        }

        for (SourceGroup sg : sources.getSourceGroups(JavaProjectConstants.SOURCES_TYPE_JAVA)) {
            javaSourceGroups.add(new JavaSourceGroup(project, sg));
        }

        for (SourceGroup sg : sources.getSourceGroups(JavaProjectConstants.SOURCES_TYPE_RESOURCES)) {
            resourceSourceGroups.add(new ResourceSourceGroup(project, sg));
        }
    }

    /** 
     * Calculates the total recursive size of the entire project structure.
     * <p>
     * Implementation details:
     * Aggregates sizes from root files, Java groups, and resource groups. 
     * Root folders are not sized individually as they are typically 
     * non-source directories (e.g., target, build).
     * </p>
     * 
     * @return Total byte count.
     */
    @Override
    public long getTotalSize() {
        long size = rootFiles.stream().mapToLong(ProjectComponent::getTotalSize).sum();
        size += javaSourceGroups.stream().mapToLong(JavaSourceGroup::getTotalSize).sum();
        size += resourceSourceGroups.stream().mapToLong(ResourceSourceGroup::getTotalSize).sum();
        return size;
    }

    /** 
     * Renders the entire project structure into a Markdown representation.
     * <p>
     * Implementation details:
     * 1. Outputs a level-2 header with the project name.
     * 2. Renders root-level items (files and folder list).
     * 3. Iteratively triggers rendering for all Java and Resource source groups.
     * </p>
     * 
     * @param sb The target StringBuilder.
     * @param indent The current indentation level.
     * @param summary If true, renders condensed aggregate views.
     */
    @Override
    public void renderMarkdown(StringBuilder sb, String indent, boolean summary) {
        sb.append(indent).append("## Project Structure: ").append(projectName).append("\n");

        if (!rootFiles.isEmpty() || !rootFolders.isEmpty()) {
            sb.append("\n").append(indent).append("### Root Directory\n");
            if (!rootFolders.isEmpty()) {
                sb.append(indent).append("  - Folders: `").append(String.join("`, `", rootFolders)).append("`\n");
            }
            for (ProjectComponent file : rootFiles) {
                file.renderMarkdown(sb, indent + "  ", summary);
            }
        }

        for (JavaSourceGroup group : javaSourceGroups) {
            group.renderMarkdown(sb, indent, summary);
        }

        for (ResourceSourceGroup group : resourceSourceGroups) {
            group.renderMarkdown(sb, indent, summary);
        }
    }
}
