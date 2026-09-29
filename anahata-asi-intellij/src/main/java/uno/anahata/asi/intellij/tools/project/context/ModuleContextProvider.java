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
import org.jetbrains.idea.maven.model.MavenId;
import org.jetbrains.idea.maven.project.MavenProject;
import org.jetbrains.idea.maven.project.MavenProjectsManager;
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
    private ProjectStructureScope scope = null;

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

        initStandardChildren(module);

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
        return buildOverview(getModule());
    }

    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        ProjectOverview ov = getOverview();
        ragMessage.addTextPart(ov.toMarkdown());
    }
}
