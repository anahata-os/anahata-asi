/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.gradle;

import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.openapi.externalSystem.model.ProjectSystemId;
import com.intellij.openapi.externalSystem.model.execution.ExternalSystemTaskExecutionSettings;
import com.intellij.openapi.externalSystem.service.execution.ProgressExecutionMode;
import com.intellij.openapi.externalSystem.settings.AbstractExternalSystemSettings;
import com.intellij.openapi.externalSystem.settings.ExternalProjectSettings;
import com.intellij.openapi.externalSystem.task.TaskCallback;
import com.intellij.openapi.externalSystem.util.ExternalSystemApiUtil;
import com.intellij.openapi.externalSystem.util.ExternalSystemUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;

import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * A toolkit for running Gradle tasks in projects linked to IntelliJ's external-system framework.
 * <p>
 * The IntelliJ counterpart of the {@code Maven} toolkit for Gradle-based projects. It uses only the
 * provider-agnostic external-system API (addressing Gradle by its {@code "GRADLE"} system id) so it
 * does not compile-depend on the Gradle plugin's own classes; the running IDE routes execution to
 * the bundled Gradle plugin (declared in {@code plugin.xml}). Tasks run via
 * {@link ExternalSystemUtil#runTask}, waiting (bounded) for completion through a {@link TaskCallback}.
 * Task console output streams to the IDE's Gradle/Run tool window.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for listing Gradle projects and running Gradle tasks.")
public class Gradle extends AnahataToolkit {

    /**
     * The external-system id for Gradle (equivalent to {@code GradleConstants.SYSTEM_ID}), built
     * directly to avoid a compile dependency on the Gradle plugin's classes.
     */
    private static final ProjectSystemId GRADLE_SYSTEM_ID = new ProjectSystemId("GRADLE");

    /**
     * Constructs the Gradle toolkit (instantiated reflectively via its public no-arg constructor).
     */
    public Gradle() {
    }

    /**
     * Lists the Gradle projects linked to any open IntelliJ project.
     *
     * @return a Markdown listing of linked Gradle project root paths.
     */
    @AgiTool("Lists the Gradle projects linked to the open IntelliJ projects.")
    public String getGradleProjects() {
        StringBuilder sb = new StringBuilder("## Linked Gradle Projects\n");
        boolean any = false;
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            AbstractExternalSystemSettings<?, ?, ?> settings = ExternalSystemApiUtil.getSettings(project, GRADLE_SYSTEM_ID);
            for (ExternalProjectSettings linked : settings.getLinkedProjectsSettings()) {
                any = true;
                sb.append("- `").append(linked.getExternalProjectPath()).append("` (").append(project.getName()).append(")\n");
            }
        }
        return any ? sb.toString() : "No linked Gradle projects found in any open project.";
    }

    /**
     * Runs one or more Gradle tasks in a linked Gradle project and waits (bounded) for completion.
     *
     * @param projectPath      the absolute path of the Gradle project root (the directory holding
     *                         {@code build.gradle}/{@code settings.gradle}).
     * @param tasks            the Gradle task names to run (e.g. {@code ["clean","build"]}).
     * @param scriptParameters optional extra command-line parameters (e.g. {@code "--info -x test"}),
     *                         or {@code null}.
     * @param timeoutSeconds   optional wait timeout in seconds (default 300); the task keeps running
     *                         in the IDE if it exceeds the wait.
     * @return a structured summary of the outcome.
     * @throws AgiToolException if no hosting project can be resolved or the wait is interrupted.
     */
    @AgiTool("Runs one or more Gradle tasks in a linked Gradle project and waits for completion (output in the Gradle tool window).")
    public String runGradleTask(
            @AgiToolParam("The absolute path of the Gradle project root.") String projectPath,
            @AgiToolParam("The Gradle task names to run, e.g. ['clean','build'].") List<String> tasks,
            @AgiToolParam(value = "Optional extra command-line parameters, e.g. '--info -x test'.", required = false) String scriptParameters,
            @AgiToolParam(value = "Optional wait timeout in seconds (default 300).", required = false) Integer timeoutSeconds) throws AgiToolException {

        Project project = resolveProject(projectPath);

        ExternalSystemTaskExecutionSettings settings = new ExternalSystemTaskExecutionSettings();
        settings.setExternalProjectPath(projectPath);
        settings.setTaskNames(tasks);
        settings.setExternalSystemIdString(GRADLE_SYSTEM_ID.getId());
        if (scriptParameters != null && !scriptParameters.isBlank()) {
            settings.setScriptParameters(scriptParameters);
        }

        CountDownLatch latch = new CountDownLatch(1);
        AtomicBoolean success = new AtomicBoolean(false);
        long startTime = System.currentTimeMillis();

        ExternalSystemUtil.runTask(settings, DefaultRunExecutor.EXECUTOR_ID, project, GRADLE_SYSTEM_ID,
                new TaskCallback() {
                    @Override
                    public void onSuccess() {
                        success.set(true);
                        latch.countDown();
                    }

                    @Override
                    public void onFailure() {
                        success.set(false);
                        latch.countDown();
                    }
                }, ProgressExecutionMode.IN_BACKGROUND_ASYNC);

        int timeout = timeoutSeconds != null && timeoutSeconds > 0 ? timeoutSeconds : 300;
        boolean finished;
        try {
            finished = latch.await(timeout, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new AgiToolException("Interrupted while waiting for Gradle tasks: " + tasks);
        }

        long durationMs = System.currentTimeMillis() - startTime;
        if (!finished) {
            return "Gradle tasks " + tasks + " launched and still running after " + timeout
                    + "s (output in the Gradle tool window).";
        }
        String outcome = success.get() ? "SUCCESS" : "FAILED";
        log("Gradle " + tasks + " -> " + outcome + " in " + durationMs + " ms");
        return "## Gradle Result: " + tasks + "\n"
                + "- **Project**: `" + projectPath + "`\n"
                + "- **Outcome**: " + outcome + "\n"
                + "- **Duration**: " + durationMs + " ms\n"
                + "(full output is in the IDE Gradle tool window)";
    }

    /**
     * Resolves the open project best associated with a Gradle project path (content-root match,
     * else the first open project).
     *
     * @param projectPath the Gradle project root path.
     * @return the hosting IntelliJ project.
     * @throws AgiToolException if no project is open.
     */
    private Project resolveProject(String projectPath) throws AgiToolException {
        String target = Path.of(projectPath).toAbsolutePath().toString();
        Project[] open = ProjectManager.getInstance().getOpenProjects();
        for (Project project : open) {
            String basePath = project.getBasePath();
            if (basePath != null && target.startsWith(Path.of(basePath).toAbsolutePath().toString())) {
                return project;
            }
        }
        if (open.length > 0) {
            return open[0];
        }
        throw new AgiToolException("No open IntelliJ project to host Gradle execution for: " + projectPath);
    }
}
