/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.internal;

import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import java.nio.file.Path;

/**
 * Shared language-agnostic utilities for locating and navigating IntelliJ projects and files.
 * <p>
 * Provides centralized project resolution without coupling callers to Java PSI APIs.
 * </p>
 *
 * @author anahata
 */
public final class ProjectUtils {

    /**
     * Non-instantiable utility class.
     */
    private ProjectUtils() {
    }

    /**
     * Resolves an absolute path to a {@link VirtualFile}, refreshing the VFS if needed.
     *
     * @param filePath The absolute filesystem path.
     * @return The virtual file, or {@code null} if it does not exist.
     */
    public static VirtualFile findVirtualFile(String filePath) {
        if (filePath == null || filePath.isBlank()) {
            return null;
        }
        return VfsUtil.findFile(Path.of(filePath), true);
    }

    /**
     * Resolves the open project whose content roots contain the given file, falling
     * back to the first open project so that files outside any project remain usable.
     *
     * @param file The file to host.
     * @return A hosting project, or {@code null} if no projects are open.
     */
    public static Project findHostProject(VirtualFile file) {
        if (file == null) {
            Project[] open = ProjectManager.getInstance().getOpenProjects();
            return open.length > 0 ? open[0] : null;
        }
        return ReadAction.computeBlocking(() -> {
            Project[] open = ProjectManager.getInstance().getOpenProjects();
            for (Project project : open) {
                if (project != null && !project.isDisposed()) {
                    try {
                        if (ProjectRootManager.getInstance(project).getFileIndex().isInContent(file)) {
                            return project;
                        }
                    } catch (Throwable ignored) {
                    }
                }
            }
            return open.length > 0 ? open[0] : null;
        });
    }
}
