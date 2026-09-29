/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.project.context;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.idea.maven.model.MavenId;
import org.jetbrains.idea.maven.project.MavenProject;
import org.jetbrains.idea.maven.project.MavenProjectsManager;
import uno.anahata.asi.agi.context.BasicContextProvider;
import uno.anahata.asi.agi.context.ContextPosition;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.intellij.internal.ProjectUtils;
import uno.anahata.asi.intellij.tools.maven.Maven;
import uno.anahata.asi.intellij.tools.project.Projects;
import uno.anahata.asi.intellij.tools.vcs.VCS;
import uno.anahata.asi.toolkit.maven.DependencyScope;
import uno.anahata.asi.toolkit.project.ProjectOverview;
import uno.anahata.asi.toolkit.project.ProjectStructureScope;

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

    /**
     * Returns the local structure scope override for this project or module node,
     * or {@code null} if this node inherits its scope from its parent.
     *
     * @return The local {@link ProjectStructureScope}, or null to inherit.
     */
    public abstract ProjectStructureScope getScope();

    /**
     * Resolves the effective {@link ProjectStructureScope} hierarchically.
     * If the local scope is {@code null}, walks up the context provider parent chain
     * until a configured scope is found, or defaults to a standard scope.
     *
     * @return The non-null effective {@link ProjectStructureScope}.
     */
    public ProjectStructureScope getEffectiveScope() {
        ProjectStructureScope local = getScope();
        if (local != null) {
            return local;
        }
        if (getParentProvider() instanceof AbstractProjectContextProvider parent) {
            return parent.getEffectiveScope();
        }
        return new ProjectStructureScope();
    }

    /**
     * Initializes and registers the standard structural children: {@link ProjectStructureContextProvider}
     * and {@link ProjectAlertsContextProvider}.
     *
     * @param targetModule The target module instance, or null for root project.
     */
    protected void initStandardChildren(Module targetModule) {
        ProjectStructureContextProvider structure = new ProjectStructureContextProvider(
                projectsToolkit, projectPath, targetModule);
        structure.setParentProvider(this);
        children.add(structure);

        ProjectAlertsContextProvider alerts = new ProjectAlertsContextProvider(
                projectsToolkit, projectPath, targetModule);
        alerts.setParentProvider(this);
        children.add(alerts);
    }
    /**
     * Builds a structured {@link ProjectOverview} model for this project or module.
     * Extracts Maven coordinates, packaging, declared dependencies, and SDK info.
     *
     * @param targetModule Optional module instance if building an overview for a module, or null for root project.
     * @return The populated {@link ProjectOverview}.
     */
    protected ProjectOverview buildOverview(Module targetModule) {
        Project p = getProject();
        String targetName = targetModule != null ? targetModule.getName() : (p != null ? p.getName() : getName());
        String packaging = targetModule != null ? "jar" : "pom";
        String mavenGroupId = null;
        String mavenArtifactId = null;
        String mavenVersion = null;

        if (p != null) {
            MavenProjectsManager mavenMgr = MavenProjectsManager.getInstance(p);
            MavenProject mp = null;
            if (targetModule != null) {
                mp = mavenMgr.findProject(targetModule);
            }
            if (mp == null) {
                VirtualFile pomVf = ProjectUtils.findVirtualFile(Path.of(projectPath).resolve("pom.xml").toString());
                if (pomVf != null) {
                    mp = mavenMgr.findProject(pomVf);
                }
            }
            if (mp != null) {
                packaging = mp.getPackaging();
                MavenId mid = mp.getMavenId();
                if (mid != null) {
                    mavenGroupId = mid.getGroupId();
                    mavenArtifactId = mid.getArtifactId();
                    mavenVersion = mid.getVersion();
                }
            }
        }

        String sdkInfo = null;
        if (targetModule != null) {
            Sdk sdk = ModuleRootManager.getInstance(targetModule).getSdk();
            if (sdk != null) {
                sdkInfo = sdk.getName() + " (" + (sdk.getVersionString() != null ? sdk.getVersionString() : "unknown") + ")";
            }
        } else if (p != null) {
            Sdk sdk = ProjectRootManager.getInstance(p).getProjectSdk();
            if (sdk != null) {
                sdkInfo = sdk.getName() + " (" + (sdk.getVersionString() != null ? sdk.getVersionString() : "unknown") + ")";
            }
        }

        List<DependencyScope> declaredDeps = null;
        try {
            declaredDeps = Maven.getDeclaredDependencies(projectPath);
        } catch (Exception e) {
            log.debug("No declared dependencies resolved for: {}", projectPath);
        }

        String vcsOverview = null;
        if (projectsToolkit.getAgi() != null) {
            Optional<VCS> vcsOpt = projectsToolkit.getAgi().getToolkit(VCS.class);
            if (vcsOpt.isPresent() && vcsOpt.get().isRepoRoot(projectPath)) {
                try {
                    vcsOverview = vcsOpt.get().getRepositoryOverview(projectPath);
                } catch (Exception e) {
                    log.debug("VCS overview not applicable for: {}", projectPath);
                }
            }
        }

        return ProjectOverview.builder()
                .id(targetName)
                .displayName(targetName)
                .projectDirectory(projectPath)
                .packaging(packaging)
                .mavenGroupId(mavenGroupId)
                .mavenArtifactId(mavenArtifactId)
                .mavenVersion(mavenVersion)
                .javaSourceLevel(sdkInfo)
                .mavenDeclaredDependencies(declaredDeps)
                .vcsOverview(vcsOverview)
                .build();
    }
}