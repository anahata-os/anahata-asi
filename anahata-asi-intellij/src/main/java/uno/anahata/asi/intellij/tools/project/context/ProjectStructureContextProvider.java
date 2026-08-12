/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.project.context;

import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.intellij.tools.project.Projects;

/**
 * Provides a source-root-aware map of a project's Java structure.
 * <p>
 * Unlike a raw filesystem walk, this provider uses the IntelliJ project model
 * ({@link ProjectRootManager#getContentSourceRoots()} + {@link ProjectFileIndex}) so the
 * ASI sees the logical layout the IDE sees: each configured source root, labelled as
 * production or test, with its package/type tree. All model and VFS access runs inside a
 * {@link ReadAction} because context is assembled on a background thread.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ProjectStructureContextProvider extends AbstractProjectContextProvider {

    /**
     * Constructs a new project structure context provider.
     *
     * @param projectsToolkit The parent Projects toolkit.
     * @param projectPath     The absolute path to the project.
     */
    public ProjectStructureContextProvider(Projects projectsToolkit, String projectPath) {
        super("structure", "Structure", "Source-root-aware project type map", projectsToolkit, projectPath);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Renders each content source root (flagged main/test) and its package/type tree,
     * built from the IntelliJ project model inside a read action.
     * </p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        Project p = getProject();
        if (p == null) {
            ragMessage.addTextPart("Couldn't fetch IntelliJ Project, project may have been closed");
            return;
        }

        String markdown = ReadAction.compute(() -> {
            StringBuilder sb = new StringBuilder();
            sb.append("  ## Project Structure: ").append(p.getName()).append("\n\n");

            ProjectRootManager rootManager = ProjectRootManager.getInstance(p);
            ProjectFileIndex fileIndex = rootManager.getFileIndex();
            VirtualFile[] sourceRoots = rootManager.getContentSourceRoots();
            if (sourceRoots.length == 0) {
                sb.append("  - No configured source roots.\n");
                return sb.toString();
            }

            for (VirtualFile root : sourceRoots) {
                boolean test = fileIndex.isInTestSourceContent(root);
                sb.append("  ### Source Root (").append(test ? "test" : "main").append("): `")
                  .append(root.getPath()).append("`\n");
                appendTree(root, sb, "    ", 0);
            }
            return sb.toString();
        });

        ragMessage.addTextPart(markdown);
    }

    /**
     * Recursively renders a source-root subtree as Markdown: directories (packages) as
     * folders and {@code .java} files as types, skipping build-output and hidden dirs and
     * capping recursion to keep the context lean.
     * <p>
     * Must be called inside a read action.
     * </p>
     *
     * @param dir   the current directory node.
     * @param sb    the Markdown accumulator.
     * @param indent the current indentation prefix.
     * @param depth  the current recursion depth.
     */
    private void appendTree(VirtualFile dir, StringBuilder sb, String indent, int depth) {
        if (depth > 12) {
            return;
        }
        VirtualFile[] children = dir.getChildren();
        for (VirtualFile child : children) {
            String name = child.getName();
            if (name.startsWith(".") || name.equals("target") || name.equals("out") || name.equals("build")) {
                continue;
            }
            if (child.isDirectory()) {
                sb.append(indent).append("- 📦 `").append(name).append("`\n");
                appendTree(child, sb, indent + "  ", depth + 1);
            } else if ("java".equals(child.getExtension())) {
                sb.append(indent).append("- 🄹 `").append(name).append("`\n");
            }
        }
    }
}
