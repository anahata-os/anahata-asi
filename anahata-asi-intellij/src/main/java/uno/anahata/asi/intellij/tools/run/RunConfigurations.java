/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.run;

import com.intellij.execution.ProgramRunnerUtil;
import com.intellij.execution.RunManager;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;

/**
 * A toolkit for discovering and launching IntelliJ run/debug/test configurations.
 * <p>
 * A beyond-parity capability with no NetBeans equivalent: it uses the platform
 * {@link RunManager}/{@link ProgramRunnerUtil} to enumerate configured run configurations and
 * execute them with the standard Run executor. Test configurations (JUnit/TestNG/etc.) run the
 * same way; their results appear in the IDE's Run/Test tool window. Capturing structured test
 * results programmatically is deferred (it requires attaching to the test process's event
 * listener).
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for listing and launching IntelliJ run/debug/test configurations.")
public class RunConfigurations extends AnahataToolkit {

    /**
     * Constructs the RunConfigurations toolkit (instantiated reflectively via its public no-arg constructor).
     */
    public RunConfigurations() {
    }

    /**
     * Lists every run configuration across all open projects.
     *
     * @return a Markdown listing of configuration names, types and owning project.
     */
    @AgiTool("Lists all run/debug/test configurations across open projects.")
    public String listRunConfigurations() {
        StringBuilder sb = new StringBuilder("## Run Configurations\n");
        boolean any = false;
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            for (RunnerAndConfigurationSettings settings : RunManager.getInstance(project).getAllSettings()) {
                any = true;
                sb.append("- **").append(settings.getName()).append("** [")
                  .append(settings.getType().getDisplayName()).append("] (").append(project.getName()).append(")\n");
            }
        }
        return any ? sb.toString() : "No run configurations found in any open project.";
    }

    /**
     * Launches a run configuration by name using the standard Run executor.
     * <p>
     * The configuration is looked up across open projects. Execution is asynchronous; its
     * console output appears in the IDE's Run tool window.
     * </p>
     *
     * @param name the exact configuration name.
     * @return a confirmation that the configuration was launched.
     * @throws AgiToolException if no configuration with that name exists.
     */
    @AgiTool("Launches a run/test configuration by name (output appears in the IDE Run tool window).")
    public String runConfiguration(
            @AgiToolParam("The exact name of the run configuration to launch.") String name) throws AgiToolException {

        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            RunnerAndConfigurationSettings settings = RunManager.getInstance(project).findConfigurationByName(name);
            if (settings != null) {
                ApplicationManager.getApplication().invokeAndWait(() ->
                        ProgramRunnerUtil.executeConfiguration(settings, DefaultRunExecutor.getRunExecutorInstance()));
                log("Launched run configuration: " + name);
                return "Launched run configuration '" + name + "' (output in the IDE Run tool window).";
            }
        }
        throw new AgiToolException("No run configuration named: " + name);
    }
}
