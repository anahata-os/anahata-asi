/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.project.context;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.intellij.tools.maven.Maven;
import uno.anahata.asi.intellij.tools.project.Projects;
import uno.anahata.asi.intellij.tools.vcs.VCS;
import uno.anahata.asi.toolkit.maven.DependencyScope;
import uno.anahata.asi.toolkit.project.ProjectOverview;
import uno.anahata.asi.toolkit.project.ProjectStructureScope;

/**
 * A hierarchical context provider for a specific IntelliJ project.
 * Consolidates metadata, actions, and project-specific instructions (anahata.md).
 * 
 * @author anahata
 */
@Slf4j
public class ProjectContextProvider extends AbstractProjectContextProvider {

    /**
     * Constructs a new root project context provider.
     * 
     * @param projectsToolkit The parent Projects toolkit.
     * @param project The IntelliJ project instance.
     */
    public ProjectContextProvider(Projects projectsToolkit, Project project) {
        super(project.getBasePath(), 
              project.getName(), 
              "Root Project Context Provider for project: " + project.getName(),
              projectsToolkit,
              project.getBasePath());
        this.project = project;
        
        // Register with parent
        this.setParentProvider(projectsToolkit);
        
        // Root structural children (for root pom, root files, root alerts)
        ProjectStructureContextProvider structure = new ProjectStructureContextProvider(
                projectsToolkit, projectPath, null, new ProjectStructureScope());
        structure.setParentProvider(this);
        children.add(structure);

        ProjectAlertsContextProvider alerts = new ProjectAlertsContextProvider(
                projectsToolkit, projectPath, null);
        alerts.setParentProvider(this);
        children.add(alerts);

        // Populate module children
        syncModules();

        // Sync anahata.md on creation
        syncMdResource();
    }

    /**
     * Synchronizes child ModuleContextProviders with the current active modules in the project.
     */
    public synchronized void syncModules() {
        Project p = getProject();
        if (p == null) return;

        Module[] modules = ModuleManager.getInstance(p).getModules();
        if (modules.length <= 1) {
            return;
        }

        List<String> currentModuleNames = new ArrayList<>();
        for (Module m : modules) {
            String modPath = ModuleContextProvider.resolveModulePath(p, m);
            if (modPath.equals(projectPath) || m.getName().equals(p.getName())) {
                continue;
            }
            currentModuleNames.add(m.getName());
            boolean exists = children.stream()
                    .anyMatch(c -> c instanceof ModuleContextProvider mcp && mcp.getModuleName().equals(m.getName()));
            if (!exists) {
                ModuleContextProvider mcp = new ModuleContextProvider(projectsToolkit, p, m);
                mcp.setParentProvider(this);
                children.add(mcp);
                log.info("Registered ModuleContextProvider for module: {}", m.getName());
            }
        }

        children.removeIf(c -> {
            if (c instanceof ModuleContextProvider mcp) {
                if (!currentModuleNames.contains(mcp.getModuleName())) {
                    log.info("Removing ModuleContextProvider for unloaded module: {}", mcp.getModuleName());
                    mcp.setProviding(false);
                    return true;
                }
            }
            return false;
        });
    }

    @Override
    public List<ContextProvider> getChildren() {
        syncModules();
        return super.getChildren();
    }

    @Override
    public String getName() {
        Project p = getProject();
        return p != null ? p.getName() : super.getName();
    }

    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        Project p = getProject();
        String vcsOverview = null;
        if (projectsToolkit.getAgi() != null) {
            Optional<VCS> vcsOpt = projectsToolkit.getAgi().getToolkit(VCS.class);
            if (vcsOpt.isPresent() && vcsOpt.get().isRepoRoot(projectPath)) {
                try {
                    vcsOverview = vcsOpt.get().getRepositoryOverview(projectPath);
                } catch (Exception e) {
                    log.debug("Could not resolve VCS overview for root {}: {}", projectPath, e.getMessage());
                }
            }
        }

        List<DependencyScope> declaredDeps = null;
        try {
            declaredDeps = Maven.getDeclaredDependencies(projectPath);
        } catch (Exception e) {
            log.debug("No root declared dependencies for: {}", projectPath);
        }

        String sdkInfo = null;
        if (p != null) {
            Sdk sdk = ProjectRootManager.getInstance(p).getProjectSdk();
            if (sdk != null) {
                sdkInfo = sdk.getName() + " (" + (sdk.getVersionString() != null ? sdk.getVersionString() : "unknown") + ")";
            }
        }

        ProjectOverview overview = ProjectOverview.builder()
                .id(p != null ? p.getName() : getName())
                .displayName(p != null ? p.getName() : getName())
                .projectDirectory(projectPath)
                .packaging("pom")
                .javaSourceLevel(sdkInfo)
                .mavenDeclaredDependencies(declaredDeps)
                .vcsOverview(vcsOverview)
                .build();

        ragMessage.addTextPart(overview.toMarkdown());
    }
}