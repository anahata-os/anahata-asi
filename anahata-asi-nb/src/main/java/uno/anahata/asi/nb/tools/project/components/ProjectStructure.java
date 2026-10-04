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
import uno.anahata.asi.ide.tools.project.ProjectStructureScope;

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
     * Warnings encountered during AST or filesystem scanning.
     */
    @Builder.Default
    private List<String> scanWarnings = new ArrayList<>();

    /**
     * The strategy used to scan and resolve structure metadata.
     */
    private ScanStrategy scanStrategy;

    /**
     * Enumerates the strategy used to scan and resolve project structure metadata.
     */
    public enum ScanStrategy {
        /**
         * Fast bytecode signature index scanning via OW2 ASM.
         */
        ASM_SIG("Fast Bytecode Signature Index (ASM)"),

        /**
         * Deep single-pass javac compiler AST scanning via JavaSource.
         */
        JAVASOURCE_AST("Full Javac Compilation AST (JavaSource)");

        private final String description;

        /**
         * Constructs a ScanStrategy.
         * 
         * @param description Human-readable description for prompt reporting.
         */
        ScanStrategy(String description) {
            this.description = description;
        }

        /**
         * Gets the human-readable description of the strategy.
         * 
         * @return The description string.
         */
        public String getDescription() {
            return description;
        }
    }

    /**
     * Builds the complete project structure recursively using a default granularity scope.
     *
     * @param project The NetBeans project instance to map.
     * @throws Exception if construction of any constituent group fails.
     */
    public ProjectStructure(Project project) throws Exception {
        this(project, new ProjectStructureScope());
    }

    /**
     * Builds the complete project structure recursively respecting the specified granularity scope.
     *
     * @param project The NetBeans project instance to map.
     * @param scope The active project structure granularity scope.
     * @throws Exception if construction of any constituent group fails.
     */
    public ProjectStructure(Project project, ProjectStructureScope scope) throws Exception {
        this.projectName = ProjectUtils.getInformation(project).getDisplayName();
        this.rootFiles = new ArrayList<>();
        this.rootFolders = new ArrayList<>();
        this.javaSourceGroups = new ArrayList<>();
        this.resourceSourceGroups = new ArrayList<>();
        this.scanWarnings = new ArrayList<>();
        this.scanStrategy = (scope != null && scope.isShowJavadoc()) ? ScanStrategy.JAVASOURCE_AST : ScanStrategy.ASM_SIG;

        FileObject root = project.getProjectDirectory();
        Sources sources = ProjectUtils.getSources(project);
        List<FileObject> sgRoots = new ArrayList<>();
        for (SourceGroup sg : sources.getSourceGroups(Sources.TYPE_GENERIC)) {
            sgRoots.add(sg.getRootFolder());
        }

        for (FileObject child : root.getChildren()) {
            if (child.isFolder()) {
                if (!sgRoots.contains(child)) {
                    if (".github".equals(child.getNameExt())) {
                        resourceSourceGroups.add(new ResourceSourceGroup(project, child, ".github"));
                    } else {
                        rootFolders.add(child.getNameExt());
                    }
                }
            } else {
                rootFiles.add(new ProjectComponent(child, null));
            }
        }

        for (SourceGroup sg : sources.getSourceGroups(JavaProjectConstants.SOURCES_TYPE_JAVA)) {
            javaSourceGroups.add(new JavaSourceGroup(project, sg, scope, scanWarnings, scanStrategy));
        }

        if (scope == null || scope.isShowResources()) {
            for (SourceGroup sg : sources.getSourceGroups(JavaProjectConstants.SOURCES_TYPE_RESOURCES)) {
                resourceSourceGroups.add(new ResourceSourceGroup(project, sg));
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Aggregates sizes from root files, Java groups, and resource groups.
     * Root folders are not sized individually as they are typically 
     * non-source directories (e.g., target, build).
     * </p>
     */
    @Override
    public long getTotalSize() {
        long size = rootFiles.stream().mapToLong(ProjectComponent::getTotalSize).sum();
        size += javaSourceGroups.stream().mapToLong(JavaSourceGroup::getTotalSize).sum();
        size += resourceSourceGroups.stream().mapToLong(ResourceSourceGroup::getTotalSize).sum();
        return size;
    }

    /**
     * {@inheritDoc}
     * <p>
     * 1. Outputs a level-2 header with the project name.
     * 2. Renders root-level items (files and folder list).
     * 3. Iteratively triggers rendering for all Java and Resource source groups.
     * </p>
     */
    @Override
    public void renderMarkdown(StringBuilder sb, String indent, ProjectStructureScope scope) {
        sb.append(indent).append("## Project Structure: ").append(projectName).append("\n");
        if (scanStrategy != null) {
            sb.append(indent).append("> Scan Strategy: ").append(scanStrategy.getDescription()).append("\n\n");
        }

        if (scope.isShowRootFiles() && (!rootFiles.isEmpty() || !rootFolders.isEmpty())) {
            sb.append("\n").append(indent).append("### Root Directory\n");
            if (!rootFolders.isEmpty()) {
                sb.append(indent).append("  - Folders: `").append(String.join("`, `", rootFolders)).append("`\n");
            }
            for (ProjectComponent file : rootFiles) {
                file.renderMarkdown(sb, indent + "  ", scope);
            }
        }

        for (JavaSourceGroup group : javaSourceGroups) {
            group.renderMarkdown(sb, indent, scope);
        }

        if (scope.isShowResources()) {
            for (ResourceSourceGroup group : resourceSourceGroups) {
                group.renderMarkdown(sb, indent, scope);
            }
        }

        if (scanWarnings != null && !scanWarnings.isEmpty()) {
            sb.append("\n").append(indent).append("> [!WARNING]\n");
            sb.append(indent).append("> Structure Scan Notice (").append(scanWarnings.size()).append(" warnings encountered):\n");
            for (String w : scanWarnings) {
                sb.append(indent).append("> - ").append(w).append("\n");
            }
            sb.append(indent).append("> Check IDE logs for full stack traces and details.\n");
        }
    }
}
