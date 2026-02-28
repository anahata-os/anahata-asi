/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.components;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.SourceGroup;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileStateInvalidException;
import org.openide.filesystems.FileUtil;

/**
 * A specialized container for a physical resource source group (e.g., src/main/resources).
 * <p>
 * This class performs a recursive physical walk of the source root, grouping 
 * files into folder-based nodes to provide a filesystem-accurate map of 
 * project assets.
 * </p>
 * 
 * @author Anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public final class ResourceSourceGroup2 extends ProjectNode2 {

    /** 
     * The display name of the source group. 
     */
    private String name;
    
    /** 
     * The physical path relative to the project root. 
     */
    private String relPath;

    /** 
     * The list of physical folders discovered within this group. 
     */
    @Builder.Default
    private List<ResourceFolder2> folders = new ArrayList<>();

    /**
     * Constructs and populates the resource source group tree.
     * 
     * @param project The parent project.
     * @param sg The NetBeans source group instance.
     * @throws Exception if the physical walk fails.
     */
    public ResourceSourceGroup2(Project project, SourceGroup sg) throws Exception {
        this.name = sg.getDisplayName();
        this.relPath = FileUtil.getRelativePath(project.getProjectDirectory(), sg.getRootFolder());
        this.folders = new ArrayList<>();

        Map<String, ResourceFolder2> dirMap = new TreeMap<>();
        walkResources(sg.getRootFolder(), sg.getRootFolder(), dirMap);
        this.folders.addAll(dirMap.values());
    }

    /**
     * Performs a recursive walk of the filesystem to identify all files 
     * and non-empty directories.
     * 
     * @param root The source group root folder.
     * @param current The current folder in the recursion.
     * @param dirMap The target map to store discovered folders.
     * @throws FileStateInvalidException if the file reference is invalid.
     */
    private void walkResources(FileObject root, FileObject current, Map<String, ResourceFolder2> dirMap) throws FileStateInvalidException {
        String path = FileUtil.getRelativePath(root, current);
        if (path == null || path.isEmpty()) {
            path = "/";
        }
        
        ResourceFolder2 folder = ResourceFolder2.builder().path(path).build();
        dirMap.put(path, folder);

        for (FileObject child : current.getChildren()) {
            if (child.isFolder()) {
                walkResources(root, child, dirMap);
            } else {
                folder.addComponent(new ProjectComponent2(child, null));
            }
        }
        
        // Remove empty folders unless they are sub-branches with files deeper down
        if (folder.getComponents().isEmpty() && !"/".equals(path)) {
            final String currentPath = path;
            boolean hasSub = dirMap.keySet().stream().anyMatch(k -> k.startsWith(currentPath + "/"));
            if (!hasSub) {
                dirMap.remove(path);
            }
        }
    }

    /** 
     * Calculates the total recursive size of all folders in this group.
     * 
     * @return Total byte count.
     */
    @Override
    public long getTotalSize() {
        return folders.stream().mapToLong(ResourceFolder2::getTotalSize).sum();
    }

    /** 
     * Renders the resource source group header and all constituent folders.
     * <p>
     * Implementation details:
     * Outputs a level-3 header for the group name and relative path, then 
     * sorts and renders each physical folder.
     * </p>
     * 
     * @param sb The target StringBuilder.
     * @param indent The current indentation level.
     * @param summary If true, renders condensed aggregate views.
     */
    @Override
    public void renderMarkdown(StringBuilder sb, String indent, boolean summary) {
        sb.append("\n").append(indent).append("### ").append(name);
        if (relPath != null && !relPath.isEmpty()) {
            sb.append(" (`").append(relPath).append("`) ");
        }
        sb.append("\n");

        if (folders.isEmpty()) {
            sb.append(indent).append("  - (Empty)\n");
            return;
        }

        folders.sort(Comparator.comparing(ResourceFolder2::getPath));

        for (ResourceFolder2 folder : folders) {
            folder.renderMarkdown(sb, indent, summary);
        }
    }
}
