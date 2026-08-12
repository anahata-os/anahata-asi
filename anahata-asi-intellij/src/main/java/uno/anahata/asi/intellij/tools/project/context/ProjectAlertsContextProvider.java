/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.project.context;

import com.intellij.ide.highlighter.JavaFileType;
import com.intellij.openapi.application.ReadAction;
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

    /**
     * Constructs a new project alerts context provider.
     * 
     * @param projectsToolkit The parent Projects toolkit.
     * @param projectPath The absolute path to the project directory.
     */
    public ProjectAlertsContextProvider(Projects projectsToolkit, String projectPath) {
        super("alerts", "Alerts", "Compiler errors and project problems", projectsToolkit, projectPath);
    }

    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        Project p = getProject();
        if (p == null) {
            ragMessage.addTextPart("  - No active project workspace loaded.");
            return;
        }

        if (DumbService.isDumb(p)) {
            ragMessage.addTextPart("  ## Project Alerts: " + p.getName() + " (indexing in progress — alerts unavailable)\n");
            return;
        }

        String markdown = ReadAction.compute(() -> {
            StringBuilder sb = new StringBuilder();
            sb.append("  ## Project Alerts: ").append(p.getName()).append("\n\n");

            WolfTheProblemSolver solver = WolfTheProblemSolver.getInstance(p);
            if (solver != null && solver.hasProblemFilesBeneath(Conditions.alwaysTrue())) {
                sb.append("  ### Files With Problems\n");
                // The public API exposes only isProblemFile(vf); enumerate Java sources
                // (bounded via the file-type index) and filter — gated by the cheap
                // hasProblemFilesBeneath check above so this only runs when problems exist.
                int count = 0;
                for (VirtualFile file : FileTypeIndex.getFiles(JavaFileType.INSTANCE, GlobalSearchScope.projectScope(p))) {
                    if (solver.isProblemFile(file)) {
                        sb.append("    - [ERROR] `").append(file.getPath()).append("` has compilation or unresolved-reference problems.\n");
                        count++;
                    }
                }
                if (count == 0) {
                    sb.append("    - [WARNING] The project contains problems outside the indexed Java sources.\n");
                }
            } else {
                sb.append("  - No compiler alerts or project problems found.\n");
            }
            return sb.toString();
        });

        ragMessage.addTextPart(markdown);
    }
}