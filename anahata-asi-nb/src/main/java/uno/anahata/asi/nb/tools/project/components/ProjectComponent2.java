/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.components;

import java.util.ArrayList;
import java.util.List;
import javax.lang.model.element.ElementKind;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.netbeans.api.java.source.ElementHandle;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileStateInvalidException;
import uno.anahata.asi.internal.TextUtils;

/**
 * A leaf or branch node representing a physical file or a logical Java type.
 * It encapsulates IDE-specific metadata like version control status (badges) 
 * and physical file size.
 * 
 * @author Anahata
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public final class ProjectComponent2 extends ProjectNode2 {

    /** 
     * The fully qualified name of the Java type. 
     * This is null for physical files that do not represent a primary Java type.
     */
    private String fqn;

    /** 
     * The Java element kind (e.g., CLASS, INTERFACE, ENUM).
     * This is null for physical files.
     */
    private ElementKind kind;
    
    /** 
     * The exact name of the file as it appears on the filesystem. 
     */
    private String fileName;
    
    /** 
     * The display name provided by the IDE's node system. 
     * This typically contains Git/SVN status indicators like "[M]" or "[A]".
     */
    private String annotatedName;
    
    /** 
     * The physical size of the associated file in bytes. 
     */
    private long size;

    /** 
     * The parent component in the hierarchy. 
     * This is used to distinguish between top-level types and inner/nested types.
     */
    @ToString.Exclude
    private ProjectComponent2 parent;

    /** 
     * The list of child components, typically representing nested or inner types. 
     */
    @Builder.Default
    private List<ProjectComponent2> children = new ArrayList<>();

    /**
     * Constructs a new project component from a physical file and an optional Java handle.
     * <p>
     * Implementation details:
     * 1. Extracts physical size and filename from the FileObject.
     * 2. Uses the DataObject/Node API to retrieve the 'Annotated' display name, 
     *    stripping any HTML tags used by the IDE for rendering badges.
     * </p>
     * 
     * @param fo The NetBeans FileObject representing the file.
     * @param handle The optional ElementHandle identifying the Java type.
     * @throws FileStateInvalidException If the file reference is no longer valid.
     */
    public ProjectComponent2(FileObject fo, ElementHandle<?> handle) throws FileStateInvalidException {
        this.fqn = (handle != null) ? handle.getQualifiedName() : null;
        this.kind = (handle != null) ? handle.getKind() : null;
        this.fileName = fo.getNameExt();
        this.size = fo.getSize();
        this.children = new ArrayList<>();
        
        try {
            org.openide.nodes.Node node = org.openide.loaders.DataObject.find(fo).getNodeDelegate();
            String html = node.getHtmlDisplayName();
            if (html != null) {
                this.annotatedName = html.replaceAll("<[^>]*>", "").trim();
            } else {
                this.annotatedName = node.getDisplayName();
            }
        } catch (Exception e) {
            this.annotatedName = fileName;
        }
    }
    
    /**
     * Establishes a parent-child relationship for nested components.
     * 
     * @param child The nested component (e.g., an inner class) to add.
     */
    public void addChild(ProjectComponent2 child) {
        child.setParent(this);
        this.children.add(child);
    }

    /** 
     * Returns the physical size of this component plus the size of all nested children.
     * 
     * @return Total byte count.
     */
    @Override
    public long getTotalSize() {
        return size + children.stream().mapToLong(ProjectComponent2::getTotalSize).sum();
    }

    /** 
     * Renders this component as a Markdown list item.
     * <p>
     * Implementation details:
     * 1. Ignores rendering if 'summary' mode is active (handled by parent containers).
     * 2. Uses the 📄 icon for top-level files.
     * 3. Appends the Java kind and any IDE status flags (e.g., Git [M]) to the name.
     * 4. Recursively triggers rendering for any nested children.
     * </p>
     * 
     * @param sb The StringBuilder to append to.
     * @param indent The current indentation string.
     * @param summary Ignored at the component level.
     */
    @Override
    public void renderMarkdown(StringBuilder sb, String indent, boolean summary) {
        if (summary) {
            return;
        }

        boolean isTopLevel = (parent == null);
        String icon = isTopLevel ? "📄 " : "";
        String simpleName = getSimpleName();
        
        String status = "";
        if (annotatedName != null && !annotatedName.isEmpty()) {
            status = annotatedName.replace(fileName != null ? fileName : simpleName, "").trim();
            if (!status.isEmpty()) {
                status = " " + status;
            }
        }

        sb.append(indent).append("- ").append(icon).append("`").append(simpleName).append("` ");
        
        if (kind != null && kind != ElementKind.PACKAGE) {
            sb.append("(").append(kind).append(") ");
        }
        
        sb.append(status);
        
        if (isTopLevel) {
            sb.append(" [").append(TextUtils.formatSize(size)).append("]");
        }
        
        sb.append("\n");

        for (ProjectComponent2 child : children) {
            child.renderMarkdown(sb, indent + "  ", false);
        }
    }

    /**
     * Resolves the simple name of the component for display.
     * <p>
     * Returns "package-info" for package descriptors, otherwise extracts 
     * the name from the FQN or physical filename.
     * </p>
     * 
     * @return The display name.
     */
    private String getSimpleName() {
        if (kind == ElementKind.PACKAGE) {
            return "package-info";
        }
        if (fqn == null) {
            return fileName;
        }
        int lastDot = fqn.lastIndexOf('.');
        return (lastDot == -1) ? fqn : fqn.substring(lastDot + 1);
    }
}
