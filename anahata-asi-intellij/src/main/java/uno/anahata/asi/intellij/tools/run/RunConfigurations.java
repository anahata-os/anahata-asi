/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.run;

import com.intellij.execution.ExecutionListener;
import com.intellij.execution.ExecutionManager;
import com.intellij.execution.ProgramRunnerUtil;
import com.intellij.execution.RunManager;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.process.ProcessListener;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Key;
import com.intellij.util.messages.MessageBusConnection;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
     * Regex pattern to detect Maven/JUnit test runner summary lines.
     */
    private static final Pattern TEST_SUMMARY_PATTERN = Pattern.compile("(?i)Tests run:\\s*\\d+.*");

    /**
     * Regex pattern to detect Maven build status lines.
     */
    private static final Pattern BUILD_STATUS_PATTERN = Pattern.compile("(?i)BUILD\\s+(SUCCESS|FAILURE).*");

    /**
     * Launches a run configuration by name using the standard Run executor.
     * <p>
     * The configuration is looked up across open projects. If a positive timeout is specified,
     * this method subscribes to {@link ExecutionManager#EXECUTION_TOPIC}, captures console
     * output, waits for process termination, and returns a structured summary including exit
     * code, duration, and test/build outcomes. Otherwise, execution runs asynchronously.
     * </p>
     *
     * @param name           the exact configuration name.
     * @param timeoutSeconds optional timeout in seconds to wait for execution to complete.
     *                       0 or {@code null} launches asynchronously without blocking.
     * @return a structured summary of the execution or a launch confirmation.
     * @throws AgiToolException if no configuration with that name exists or waiting is interrupted.
     */
    @AgiTool("Launches a run/test configuration by name (output appears in the IDE Run tool window).")
    public String runConfiguration(
            @AgiToolParam("The exact name of the run configuration to launch.") String name,
            @AgiToolParam(value = "Optional timeout in seconds to wait for execution to complete and return structured summary. 0 or null runs asynchronously without waiting.", required = false) Integer timeoutSeconds) throws AgiToolException {

        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            RunnerAndConfigurationSettings settings = RunManager.getInstance(project).findConfigurationByName(name);
            if (settings != null) {
                if (timeoutSeconds != null && timeoutSeconds > 0) {
                    return executeWithStructuredWait(project, settings, timeoutSeconds);
                }
                ApplicationManager.getApplication().invokeAndWait(() ->
                        ProgramRunnerUtil.executeConfiguration(settings, DefaultRunExecutor.getRunExecutorInstance()));
                log("Launched run configuration: " + name);
                return "Launched run configuration '" + name + "' (output in the IDE Run tool window).";
            }
        }
        throw new AgiToolException("No run configuration named: " + name);
    }

    /**
     * Executes a configuration synchronously while capturing process output and waiting for termination.
     *
     * @param project        the host project.
     * @param settings       the configuration settings.
     * @param timeoutSeconds timeout duration in seconds.
     * @return structured execution summary.
     * @throws AgiToolException if execution wait fails.
     */
    private String executeWithStructuredWait(Project project, RunnerAndConfigurationSettings settings, int timeoutSeconds) throws AgiToolException {
        CountDownLatch terminatedLatch = new CountDownLatch(1);
        AtomicInteger exitCodeHolder = new AtomicInteger(-1);
        StringBuilder outputCollector = new StringBuilder();
        StringBuilder testSummaries = new StringBuilder();
        long startTime = System.currentTimeMillis();

        MessageBusConnection connection = project.getMessageBus().connect();
        try {
            connection.subscribe(ExecutionManager.EXECUTION_TOPIC, new ExecutionListener() {
                @Override
                public void processStarted(String executorId, ExecutionEnvironment env, ProcessHandler handler) {
                    if (env.getRunnerAndConfigurationSettings() == settings) {
                        handler.addProcessListener(new ProcessListener() {
                            @Override
                            public void onTextAvailable(ProcessEvent event, Key outputType) {
                                String text = event.getText();
                                if (text != null) {
                                    outputCollector.append(text);
                                    Matcher testMatcher = TEST_SUMMARY_PATTERN.matcher(text.trim());
                                    if (testMatcher.matches()) {
                                        testSummaries.append("- ").append(testMatcher.group()).append("\n");
                                    }
                                    Matcher buildMatcher = BUILD_STATUS_PATTERN.matcher(text.trim());
                                    if (buildMatcher.matches()) {
                                        testSummaries.append("- Status: ").append(buildMatcher.group()).append("\n");
                                    }
                                }
                            }

                            @Override
                            public void processTerminated(ProcessEvent event) {
                                exitCodeHolder.set(event.getExitCode());
                                terminatedLatch.countDown();
                            }
                        });
                    }
                }
            });

            ApplicationManager.getApplication().invokeAndWait(() ->
                    ProgramRunnerUtil.executeConfiguration(settings, DefaultRunExecutor.getRunExecutorInstance()));

            log("Awaiting completion of run configuration '" + settings.getName() + "' (timeout: " + timeoutSeconds + "s)...");
            boolean finished = terminatedLatch.await(timeoutSeconds, TimeUnit.SECONDS);
            long durationMs = System.currentTimeMillis() - startTime;

            if (!finished) {
                return "Run configuration '" + settings.getName() + "' was launched and is still running after "
                        + timeoutSeconds + "s (output streaming in the IDE Run tool window).";
            }

            int exitCode = exitCodeHolder.get();
            StringBuilder result = new StringBuilder();
            result.append("## Execution Result: ").append(settings.getName()).append("\n")
                  .append("- **Project**: ").append(project.getName()).append("\n")
                  .append("- **Exit Code**: ").append(exitCode).append(exitCode == 0 ? " (SUCCESS)" : " (FAILED)").append("\n")
                  .append("- **Duration**: ").append(durationMs).append(" ms\n");

            if (testSummaries.length() > 0) {
                result.append("### Test / Build Summary\n").append(testSummaries);
            } else {
                String fullOutput = outputCollector.toString().trim();
                if (!fullOutput.isEmpty()) {
                    String[] lines = fullOutput.split("\n");
                    int maxTail = Math.min(lines.length, 10);
                    result.append("### Output Tail (last ").append(maxTail).append(" lines)\n```\n");
                    for (int i = lines.length - maxTail; i < lines.length; i++) {
                        result.append(lines[i]).append("\n");
                    }
                    result.append("```\n");
                }
            }

            String summaryStr = result.toString();
            log(summaryStr);
            return summaryStr;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new AgiToolException("Execution wait was interrupted for: " + settings.getName());
        } finally {
            connection.disconnect();
        }
    }
}
