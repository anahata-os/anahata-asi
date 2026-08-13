/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.ide.nodes;

import com.intellij.icons.AllIcons;
import com.intellij.ide.projectView.PresentationData;
import com.intellij.ide.projectView.TreeStructureProvider;
import com.intellij.ide.projectView.ViewSettings;
import com.intellij.ide.util.treeView.AbstractTreeNode;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Surfaces the project's {@code anahata.md} instructions file as a branded node at the top
 * of the Project view.
 * <p>
 * This is the IntelliJ analogue of the NetBeans {@code AnahataNodeFactory}: instead of a
 * {@code NodeFactory} it hooks the {@link TreeStructureProvider} extension point and prepends
 * a navigable node to the project root's children when {@code anahata.md} exists.
 * </p>
 *
 * @author anahata
 */
public class AnahataTreeStructureProvider implements TreeStructureProvider {

    /**
     * Constructs the tree-structure provider (instantiated by the platform via its public no-arg constructor).
     */
    public AnahataTreeStructureProvider() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Prepends the Anahata instructions node to the project root node's children when the
     * project has an {@code anahata.md} in its base directory.
     * </p>
     */
    @Override
    public Collection<AbstractTreeNode<?>> modify(AbstractTreeNode<?> parent, Collection<AbstractTreeNode<?>> children, ViewSettings settings) {
        if (!(parent.getValue() instanceof Project project)) {
            return children;
        }
        VirtualFile md = findAnahataMd(project);
        if (md == null) {
            return children;
        }
        List<AbstractTreeNode<?>> result = new ArrayList<>(children);
        result.add(0, new AnahataNode(project, md));
        return result;
    }

    /**
     * Locates the {@code anahata.md} file in a project's base directory.
     *
     * @param project the project.
     * @return the file, or {@code null} if absent or the base path is unknown.
     */
    private VirtualFile findAnahataMd(Project project) {
        String basePath = project.getBasePath();
        if (basePath == null) {
            return null;
        }
        return VfsUtil.findFile(Path.of(basePath, "anahata.md"), false);
    }

    /**
     * A navigable Project-view node representing the project's {@code anahata.md} instructions.
     */
    private static final class AnahataNode extends AbstractTreeNode<VirtualFile> {

        /**
         * Creates the node for the given instructions file.
         *
         * @param project the owning project.
         * @param file    the {@code anahata.md} file.
         */
        private AnahataNode(Project project, VirtualFile file) {
            super(project, file);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Collection<? extends AbstractTreeNode<?>> getChildren() {
            return List.of();
        }

        /**
         * {@inheritDoc}
         * <p>
         * Presents the file under a branded label with the Anahata marker icon.
         * </p>
         */
        @Override
        protected void update(PresentationData presentation) {
            presentation.setPresentableText("Anahata Instructions (" + getValue().getName() + ")");
            presentation.setIcon(AllIcons.General.Balloon);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void navigate(boolean requestFocus) {
            new OpenFileDescriptor(getProject(), getValue()).navigate(requestFocus);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean canNavigate() {
            return true;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean canNavigateToSource() {
            return true;
        }
    }
}
