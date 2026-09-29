/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.project.context;

import com.intellij.ide.highlighter.JavaFileType;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Conditions;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.problems.WolfTheProblemSolver;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.intellij.tools.project.Projects;

/**
 * Provides a real-time list of project compiler errors and alerts.
 * Uses IntelliJ's native WolfTheProblemSolver to resolve files in error.
 * 
 * @author anahata
 */
@Slf4j
public class ProjectAlertsContextProvider extends AbstractProjectContextProvider {

    private transient Module module;
    private final String moduleName;

    /**
     * Constructs a new project alerts context provider.
     * 
     * @param projectsToolkit The parent Projects toolkit.
     * @param projectPath The absolute path to the project directory.
     */
    public ProjectAlertsContextProvider(Projects projectsToolkit, String projectPath) {
        this(projectsToolkit, projectPath, null);
    }

    /**
     * Constructs a new alerts context provider scoped to a specific module or project.
     *
     * @param projectsToolkit The parent Projects toolkit.
     * @param projectPath The absolute path to the project or module directory.
     * @param module The optional module to scope alerts to, or null for project-wide.
     */
    public ProjectAlertsContextProvider(Projects projectsToolkit, String projectPath, Module module) {
        super("alerts", "Alerts",
              module != null ? "Compiler errors and problems for module: " + module.getName()
                             : "Compiler errors and project problems",
              projectsToolkit, projectPath);
        this.module = module;
        this.moduleName = module != null ? module.getName() : null;
        if (module != null && !module.isDisposed()) {
            this.project = module.getProject();
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Alerts providers do not manage local structure scopes; returns null to inherit.
     * </p>
     */
    @Override
    public uno.anahata.asi.toolkit.project.ProjectStructureScope getScope() {
        return null;
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

    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        Project p = getProject();
        if (p == null) {
            ragMessage.addTextPart("  - No active project workspace loaded.");
            return;
        }

        Module m = getModule();
        if (DumbService.isDumb(p)) {
            ragMessage.addTextPart("  ## " + (m != null ? "Module" : "Project") + " Alerts: "
                    + (m != null ? m.getName() : p.getName()) + " (indexing in progress — alerts unavailable)\n");
            return;
        }

        String markdown = ReadAction.computeBlocking(() -> {
            StringBuilder sb = new StringBuilder();
            if (m != null) {
                sb.append("  ## Module Alerts: ").append(m.getName()).append("\n\n");
            } else {
                sb.append("  ## Project Alerts: ").append(p.getName()).append("\n\n");
            }

            WolfTheProblemSolver solver = WolfTheProblemSolver.getInstance(p);
            if (solver != null && solver.hasProblemFilesBeneath(Conditions.alwaysTrue())) {
                sb.append("  ### Files With Problems\n");
                GlobalSearchScope scope = (m != null)
                        ? GlobalSearchScope.moduleScope(m)
                        : GlobalSearchScope.projectScope(p);
                int count = 0;
                for (VirtualFile file : FileTypeIndex.getFiles(JavaFileType.INSTANCE, scope)) {
                    if (solver.isProblemFile(file)) {
                        sb.append("    - [ERROR] `").append(file.getPath()).append("` has compilation or unresolved-reference problems.\n");
                        count++;
                    }
                }
                if (count == 0) {
                    sb.append("    - [WARNING] The ").append(m != null ? "module" : "project")
                      .append(" contains problems outside the indexed Java sources.\n");
                }
            } else {
                sb.append("  - No compiler alerts or project problems found.\n");
            }
            return sb.toString();
        });

        ragMessage.addTextPart(markdown);
    }
}