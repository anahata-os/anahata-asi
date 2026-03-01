/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
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
public final class ProjectComponent extends ProjectNode {

    /** 
     * The fully qualified name of the Java type. 
     * This is null for physical files that do not represent a primary Java type.
     */
    private String fqn;

    /** 
     * The Java element kind (e.g., CLASS, INTERFACE, ENUM, PACKAGE).
     */
    private ElementKind kind;
    
    /** 
     * The display name provided by the IDE's node system. 
     * This typically contains Git/SVN status indicators like "[M]" or "[A]".
     */
    private String annotatedName;

    /**
     * The underlying NetBeans FileObject.
     */
    @ToString.Exclude
    private transient FileObject fileObject;

    /** 
     * The parent component in the hierarchy. 
     * This is used to distinguish between top-level types and inner/nested types.
     */
    @ToString.Exclude
    private ProjectComponent parent;

    /** 
     * The list of child components, typically representing nested or inner types. 
     */
    @Builder.Default
    private List<ProjectComponent> children = new ArrayList<>();

    /**
     * Constructs a new project component from a physical file and an optional Java handle.
     * <p>
     * Implementation details:
     * 1. Extracts physical size and filename from the FileObject.
     * 2. If the file is 'package-info.java', it is explicitly marked as ElementKind.PACKAGE.
     * 3. Uses the DataObject/Node API to retrieve the 'Annotated' display name, 
     *    stripping any HTML tags used by the IDE for rendering badges.
     * </p>
     * 
     * @param fo The NetBeans FileObject representing the file.
     * @param handle The optional ElementHandle identifying the Java type.
     * @throws FileStateInvalidException If the file reference is no longer valid.
     */
    public ProjectComponent(FileObject fo, ElementHandle<?> handle) throws FileStateInvalidException {
        this.fileObject = fo;
        this.fqn = (handle != null) ? handle.getQualifiedName() : null;
        this.kind = (handle != null) ? handle.getKind() : null;
        this.children = new ArrayList<>();
        
        // Explicitly treat package-info as ElementKind.PACKAGE
        if (fo.getNameExt().startsWith("package-info")) {
            this.kind = ElementKind.PACKAGE;
        }
        
        try {
            org.openide.nodes.Node node = org.openide.loaders.DataObject.find(fo).getNodeDelegate();
            String html = node.getHtmlDisplayName();
            if (html != null) {
                this.annotatedName = html.replaceAll("<[^>]*>", "").trim();
            } else {
                this.annotatedName = node.getDisplayName();
            }
        } catch (Exception e) {
            this.annotatedName = fo.getNameExt();
        }
    }
    
    /**
     * Establishes a parent-child relationship for nested components.
     * 
     * @param child The nested component (e.g., an inner class) to add.
     */
    public void addChild(ProjectComponent child) {
        child.setParent(this);
        this.children.add(child);
    }

    /**
     * Gets the name of the file associated with this component.
     * 
     * @return The filename with extension, or null if no file is associated.
     */
    public String getFileName() {
        return (fileObject != null) ? fileObject.getNameExt() : null;
    }

    /** 
     * Returns the physical size of this component plus the size of all nested children.
     * 
     * @return Total byte count.
     */
    @Override
    public long getTotalSize() {
        long baseSize = (fileObject != null) ? fileObject.getSize() : 0;
        return baseSize + children.stream().mapToLong(ProjectComponent::getTotalSize).sum();
    }

    /** 
     * Renders this component as a Markdown list item.
     * <p>
     * Implementation details:
     * 1. Renders the component name with a 📄 icon if it is a top-level file.
     * 2. Appends the Java kind and any IDE status flags (e.g., Git [M]) to the name.
     * 3. Recursively triggers rendering for any nested children.
     * </p>
     * 
     * @param sb The StringBuilder to append to.
     * @param indent The current indentation string.
     * @param summary If true, the parent container is handling the aggregation. 
     *                However, root-level files (parent == null) should still render.
     */
    @Override
    public void renderMarkdown(StringBuilder sb, String indent, boolean summary) {
        // We only skip rendering for components with parents in summary mode
        // to allow root-level files to always show.
        if (summary && parent != null) {
             return;
        }

        boolean isTopLevelFile = (parent == null);
        String icon = isTopLevelFile ? "📄 " : "";
        String simpleName = getSimpleName();
        String fileName = getFileName();
        if (fileName == null) {
            fileName = simpleName;
        }
        
        String status = "";
        if (annotatedName != null && !annotatedName.isEmpty()) {
            status = annotatedName.replace(fileName, "").trim();
            if (!status.isEmpty()) {
                status = " " + status;
            }
        }

        sb.append(indent).append("- ").append(icon).append("`").append(simpleName).append("` ");
        
        if (kind != null) {
            sb.append("(").append(kind.name()).append(") ");
        }
        
        sb.append(status);
        
        // Show size only for top-level physical files
        if (isTopLevelFile && fileObject != null) {
            sb.append(" [").append(TextUtils.formatSize(fileObject.getSize())).append("]");
        }
        
        sb.append("\n");

        for (ProjectComponent child : children) {
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
    public String getSimpleName() {
        String fileName = getFileName();
        if (kind == ElementKind.PACKAGE || (fileName != null && fileName.startsWith("package-info"))) {
            return "package-info";
        }
        if (fqn == null) {
            return (fileName != null) ? fileName : "unknown";
        }
        int lastDot = fqn.lastIndexOf('.');
        return (lastDot == -1) ? fqn : fqn.substring(lastDot + 1);
    }
    
    /**
     * Gets a displayable type string for this component.
     * <p>
     * For Java types, it returns the ElementKind string. For generic files, 
     * it returns the file extension in lowercase.
     * </p>
     * 
     * @return The type identifier.
     */
    public String getComponentType() {
        if (kind != null) {
            return kind.name();
        }
        String fileName = getFileName();
        if (fileName != null && fileName.contains(".")) {
            return fileName.substring(fileName.lastIndexOf('.') + 1).toLowerCase();
        }
        return "file";
    }
}
