/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.project.context;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.vfs.VirtualFile;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.context.BasicContextProvider;
import uno.anahata.asi.agi.context.ContextPosition;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.intellij.internal.ProjectUtils;
import uno.anahata.asi.intellij.tools.project.Projects;

/**
 * Common base class for context providers that are bound to a specific IntelliJ project.
 * Centralizes project resolution, toolkit access, and IDE UI synchronization logic.
 * 
 * @author anahata
 */
@Slf4j
public abstract class AbstractProjectContextProvider extends BasicContextProvider {

    /** The parent Projects toolkit instance. */
    protected final Projects projectsToolkit;
    
    /** The absolute canonical path to the project root. */
    @Getter
    protected final String projectPath;

    /** 
     * The cached IntelliJ project instance. 
     * Marked transient as it cannot be serialized directly.
     */
    protected transient Project project;

    /**
     * Constructs a new project-bound context provider.
     * 
     * @param id The unique identifier for this provider.
     * @param name The human-readable name.
     * @param description A brief description of the provided context.
     * @param projectsToolkit The parent Projects toolkit.
     * @param projectPath The absolute path to the project.
     */
    public AbstractProjectContextProvider(String id, String name, String description, Projects projectsToolkit, String projectPath) {
        super(id, name, description);
        this.projectsToolkit = projectsToolkit;
        this.projectPath = projectPath;
    }

    /**
     * Resolves the IntelliJ Project instance, restoring it from the path if needed.
     * 
     * @return The Project instance, or null if the project is no longer open.
     */
    public Project getProject() {
        if (project != null && !project.isDisposed()) {
            return project;
        }
        if (projectPath != null) {
            VirtualFile vf = ProjectUtils.findVirtualFile(projectPath);
            if (vf != null) {
                Project p = ProjectUtils.findHostProject(vf);
                if (p != null && !p.isDisposed()) {
                    project = p;
                    return project;
                }
            }
            Path targetPath = Path.of(projectPath).toAbsolutePath();
            for (Project p : ProjectManager.getInstance().getOpenProjects()) {
                if (p != null && !p.isDisposed()) {
                    String basePath = p.getBasePath();
                    if (basePath != null) {
                        Path pPath = Path.of(basePath).toAbsolutePath();
                        if (targetPath.startsWith(pPath)) {
                            project = p;
                            return project;
                        }
                    }
                }
            }
        }
        Project[] open = ProjectManager.getInstance().getOpenProjects();
        if (open.length > 0 && !open[0].isDisposed()) {
            project = open[0];
            return project;
        }
        return null;
    }

    @Override
    public void setProviding(boolean enabled) {
        super.setProviding(enabled);
        syncMdResource();
    }

    /**
     * Synchronizes the project or module's {@code anahata.md} instructions file with the session's resource
     * manager to reflect this provider's active state.
     * <p>
     * When providing, ensures {@code anahata.md} exists (creating a stub if needed) and registers it
     * at the {@code SYSTEM_INSTRUCTIONS} context position; when not providing, unregisters it.
     * </p>
     */
    protected void syncMdResource() {
        if (projectPath == null) return;
        String mdPath = Path.of(projectPath).resolve("anahata.md").toAbsolutePath().toString();
        if (projectsToolkit.getAgi() == null || projectsToolkit.getAgi().getResourceManager() == null) return;
        Optional<Resource> existing = projectsToolkit.getAgi().getResourceManager().findByPath(mdPath);

        if (isProviding()) {
            if (existing.isEmpty()) {
                try {
                    Path path = Path.of(mdPath);
                    if (!Files.exists(path)) {
                        Files.writeString(path, "# Project Instructions: " + getName() + "\n\nThis file contains project-specific system instructions.\n");
                    }
                    
                    List<Resource> registered = projectsToolkit.getAgi().getResourceManager().registerPaths(
                        List.of(path), 
                        "added to context by user via project instructions sync"
                    );
                    
                    if (!registered.isEmpty()) {
                        Resource resource = registered.get(0);
                        resource.setContextPosition(ContextPosition.SYSTEM_INSTRUCTIONS);
                        log.info("Registered anahata.md as SYSTEM_INSTRUCTIONS for: {}", projectPath);
                    }
                } catch (Exception e) {
                    log.error("Failed to sync anahata.md for path: " + projectPath, e);
                }
            } else {
                existing.get().setContextPosition(ContextPosition.SYSTEM_INSTRUCTIONS);
            }
        } else {
            existing.ifPresent(resource -> {
                projectsToolkit.getAgi().getResourceManager().unregister(resource.getId());
                log.info("Unregistered anahata.md for path: {}", projectPath);
            });
        }
    }
}