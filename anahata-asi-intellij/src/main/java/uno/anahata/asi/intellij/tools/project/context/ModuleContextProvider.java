/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.project.context;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.intellij.tools.maven.Maven;
import uno.anahata.asi.intellij.tools.project.Projects;
import uno.anahata.asi.intellij.tools.vcs.VCS;
import uno.anahata.asi.toolkit.maven.DependencyScope;
import uno.anahata.asi.toolkit.project.ProjectOverview;
import uno.anahata.asi.toolkit.project.ProjectStructureScope;

/**
 * Context provider representing an individual module in an IntelliJ project.
 * <p>
 * Each module manages its own {@link ProjectOverview}, {@link ProjectStructureScope},
 * structural AST type tree ({@link ProjectStructureContextProvider}), scoped alerts
 * ({@link ProjectAlertsContextProvider}), and project-specific instructions (anahata.md).
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ModuleContextProvider extends AbstractProjectContextProvider {

    @Getter
    private final String moduleName;

    private transient Module module;

    @Getter @Setter
    private ProjectStructureScope scope;

    /**
     * Constructs a new module context provider.
     *
     * @param projectsToolkit The parent Projects toolkit.
     * @param project The parent IntelliJ project instance.
     * @param module The IntelliJ module instance.
     */
    public ModuleContextProvider(Projects projectsToolkit, Project project, Module module) {
        super(module.getName(),
              module.getName(),
              "Module Context Provider for module: " + module.getName(),
              projectsToolkit,
              resolveModulePath(project, module));
        this.project = project;
        this.module = module;
        this.moduleName = module.getName();
        this.scope = new ProjectStructureScope();

        ProjectStructureContextProvider structure = new ProjectStructureContextProvider(
                projectsToolkit, projectPath, module, scope);
        structure.setParentProvider(this);
        children.add(structure);

        ProjectAlertsContextProvider alerts = new ProjectAlertsContextProvider(
                projectsToolkit, projectPath, module);
        alerts.setParentProvider(this);
        children.add(alerts);

        syncMdResource();
    }

    /**
     * Resolves the active IntelliJ Module instance, restoring it from name if needed.
     * 
     * @return The active Module, or null if unconfigured or unloaded.
     */
    public Module getModule() {
        if (module != null && !module.isDisposed()) {
            return module;
        }
        if (moduleName != null) {
            Project p = getProject();
            if (p != null && !p.isDisposed()) {
                module = ModuleManager.getInstance(p).findModuleByName(moduleName);
            }
        }
        return module;
    }

    /**
     * Resolves the filesystem root path for a module.
     * 
     * @param project The parent IntelliJ project.
     * @param module The target module.
     * @return The absolute path to the module directory.
     */
    public static String resolveModulePath(Project project, Module module) {
        VirtualFile[] cr = ModuleRootManager.getInstance(module).getContentRoots();
        if (cr.length > 0 && cr[0] != null) {
            return cr[0].getPath();
        }
        return project.getBasePath();
    }

    /**
     * Generates a structured {@link ProjectOverview} model for this module.
     *
     * @return The populated ProjectOverview DTO.
     */
    public ProjectOverview getOverview() {
        String packaging = "jar";
        Path pomPath = Path.of(projectPath).resolve("pom.xml");
        if (Files.exists(pomPath)) {
            try {
                String content = Files.readString(pomPath);
                if (content.contains("<packaging>pom</packaging>")) {
                    packaging = "pom";
                } else if (content.contains("<packaging>nbm</packaging>")) {
                    packaging = "nbm";
                }
            } catch (Exception e) {
                // ignore
            }
        }

        List<DependencyScope> declaredDeps = null;
        try {
            declaredDeps = Maven.getDeclaredDependencies(projectPath);
        } catch (Exception e) {
            log.debug("No declared dependencies resolved for module: {}", moduleName);
        }

        String vcsOverview = null;
        if (projectsToolkit.getAgi() != null) {
            Optional<VCS> vcsOpt = projectsToolkit.getAgi().getToolkit(VCS.class);
            if (vcsOpt.isPresent() && vcsOpt.get().isRepoRoot(projectPath)) {
                try {
                    vcsOverview = vcsOpt.get().getRepositoryOverview(projectPath);
                } catch (Exception e) {
                    log.debug("VCS overview not applicable for module: {}", moduleName);
                }
            }
        }

        return ProjectOverview.builder()
                .id(moduleName)
                .displayName(moduleName)
                .projectDirectory(projectPath)
                .packaging(packaging)
                .mavenDeclaredDependencies(declaredDeps)
                .vcsOverview(vcsOverview)
                .build();
    }

    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        ProjectOverview ov = getOverview();
        ragMessage.addTextPart(ov.toMarkdown());
    }
}
