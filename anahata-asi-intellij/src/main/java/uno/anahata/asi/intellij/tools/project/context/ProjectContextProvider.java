/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.project.context;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import java.util.ArrayList;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.intellij.tools.project.Projects;
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

    @lombok.Getter @lombok.Setter
    private ProjectStructureScope scope = new ProjectStructureScope();

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
        initStandardChildren(null);

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
        ProjectOverview overview = buildOverview(null);
        ragMessage.addTextPart(overview.toMarkdown());
    }
}