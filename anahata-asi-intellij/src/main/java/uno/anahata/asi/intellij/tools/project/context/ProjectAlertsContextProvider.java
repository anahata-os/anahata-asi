/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.project.context;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.problems.WolfTheProblemSolver;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.RagMessage;
import com.intellij.openapi.util.Conditions;
import uno.anahata.asi.intellij.tools.project.Projects;

import java.util.Collection;

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

        StringBuilder sb = new StringBuilder();
        sb.append("  ## Project Alerts: ").append(p.getName()).append("\n\n");

        WolfTheProblemSolver solver = WolfTheProblemSolver.getInstance(p);
        if (solver != null && solver.hasProblemFilesBeneath(Conditions.alwaysTrue())) {
            sb.append("  ### Java Compiler Alerts\n");
            try {
                java.lang.reflect.Method getProblemsMethod = solver.getClass().getMethod("getProblemFiles");
                @SuppressWarnings("unchecked")
                Collection<VirtualFile> files = (Collection<VirtualFile>) getProblemsMethod.invoke(solver);
                if (files != null && !files.isEmpty()) {
                    for (VirtualFile file : files) {
                        sb.append("    - [ERROR] `").append(file.getPath()).append("` has compilation or unresolved reference problems.\n");
                    }
                } else {
                    sb.append("    - [WARNING] Files contain compilation errors, but individual file lists are currently restricted.\n");
                }
            } catch (Exception e) {
                sb.append("    - [ERROR] Project has compile-time errors flagged by WolfTheProblemSolver.\n");
            }
        } else {
            sb.append("  - No compiler alerts or project problems found.\n");
        }

        ragMessage.addTextPart(sb.toString());
    }
}