/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.maven;

import com.intellij.execution.process.ProcessEvent;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.process.ProcessListener;
import com.intellij.execution.process.ProcessOutputType;
import com.intellij.lang.xml.XMLLanguage;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiParserFacade;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.xml.XmlTag;
import com.intellij.util.Consumer;
import com.intellij.util.xml.reflect.DomCollectionChildDescription;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.idea.maven.execution.MavenRunner;
import org.jetbrains.idea.maven.execution.MavenRunnerParameters;
import org.jetbrains.idea.maven.execution.MavenRunnerSettings;
import org.jetbrains.idea.maven.indices.MavenArtifactSearchResult;
import org.jetbrains.idea.maven.indices.MavenArtifactSearcher;
import org.jetbrains.idea.maven.model.MavenArtifact;
import org.jetbrains.idea.maven.model.MavenId;
import org.jetbrains.idea.maven.project.MavenProject;
import org.jetbrains.idea.maven.project.MavenProjectsManager;
import org.jetbrains.idea.maven.utils.MavenArtifactUtil;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.ToolContext;
import uno.anahata.asi.intellij.internal.ProjectUtils;
import uno.anahata.asi.toolkit.maven.AddDependencyResult;
import uno.anahata.asi.toolkit.maven.DeclaredArtifact;
import uno.anahata.asi.toolkit.maven.DependencyGroup;
import uno.anahata.asi.toolkit.maven.DependencyScope;
import uno.anahata.asi.toolkit.maven.MavenBuildResult;
import uno.anahata.asi.toolkit.maven.MavenBuildResult.ProcessStatus;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import org.apache.maven.artifact.versioning.ComparableVersion;
import org.jetbrains.idea.maven.dom.MavenDomUtil;
import org.jetbrains.idea.maven.dom.model.MavenDomDependencies;
import org.jetbrains.idea.maven.dom.model.MavenDomDependency;
import org.jetbrains.idea.maven.dom.model.MavenDomExclusion;
import org.jetbrains.idea.maven.dom.model.MavenDomExclusions;
import org.jetbrains.idea.maven.dom.model.MavenDomProjectModel;
import org.jetbrains.idea.maven.indices.MavenGAVIndex;
import org.jetbrains.idea.maven.indices.MavenIndicesManager;
import org.jetbrains.idea.maven.model.MavenRemoteRepository;
import org.jetbrains.idea.maven.model.MavenRepoArtifactInfo;
import org.jetbrains.idea.maven.project.MavenGeneralSettings;
import uno.anahata.asi.agi.message.RagMessage;

/**
 * A toolkit for inspecting and building Maven projects through the IntelliJ IDEA Maven
 * integration.
 * <p>
 * This is the IntelliJ port of the NetBeans {@code Maven} toolkit. It uses
 * {@link MavenProjectsManager} to enumerate imported Maven projects and their resolved
 * dependencies, and {@link MavenRunner} to execute goals against a project's live
 * configuration. Goal execution is asynchronous in the platform; this toolkit awaits
 * completion on a latch so the model receives a definitive result. Build output streams to
 * the IDE's Maven Run console (the platform does not expose it as a return value here).
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for inspecting and building Maven projects in IntelliJ IDEA.")
public class IntellijMaven extends AnahataToolkit {

    /** Maximum number of output lines to keep in stdOutput in the DTO (head lines). */
    private static final int MAX_OUTPUT_HEAD_LINES = 25;

    /** Maximum number of output lines to keep in stdOutput in the DTO (tail lines). */
    private static final int MAX_OUTPUT_TAIL_LINES = 75;

    /** Default timeout for Maven build execution in seconds (15 minutes). */
    private static final int DEFAULT_TIMEOUT_SECONDS = 900;

    /**
     * Constructs the Maven toolkit (instantiated reflectively via its public no-arg constructor).
     */
    public IntellijMaven() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Notes that projects must be imported as Maven projects and that goal output appears in
     * the IDE Maven console.
     * </p>
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList(
                "The Maven toolkit inspects and builds Maven projects that are imported in IntelliJ. "
                + "Use getDependencies to inspect a project's resolved classpath, "
                + "and runGoals to execute Maven goals (output streams to the IDE Maven Run console).");
    }

    /**
     * {@inheritDoc}
     * <p>
     * Populates the RAG message with live IntelliJ Maven configuration, active local and remote
     * repositories, background repository index status, and currently imported Maven projects.
     * </p>
     *
     * @param ragMessage the turn's RAG message accumulator.
     */
    @Override
    public void populateMessage(RagMessage ragMessage) {
        Project[] openProjects = ProjectManager.getInstance().getOpenProjects();
        if (openProjects.length == 0) {
            return;
        }
        Project project = openProjects[0];
        MavenProjectsManager projMgr = MavenProjectsManager.getInstance(project);
        if (projMgr == null) {
            return;
        }
        MavenIndicesManager indicesMgr = MavenIndicesManager.getInstance(project);

        StringBuilder sb = new StringBuilder("## IntelliJ Maven Configuration & Runtime\n");
        MavenGeneralSettings settings = projMgr.getGeneralSettings();
        if (settings != null) {
            sb.append("- **Maven Home**: ").append(settings.getMavenHomeType() != null ? settings.getMavenHomeType() : "Default").append("\n");
            String localRepo = settings.getLocalRepository();
            if (localRepo == null || localRepo.isBlank()) {
                localRepo = System.getProperty("user.home") + File.separator + ".m2" + File.separator + "repository";
            }
            sb.append("- **Local Repository**: `").append(localRepo).append("`\n");
            String userSettings = settings.getUserSettingsFile();
            if (userSettings == null || userSettings.isBlank()) {
                userSettings = System.getProperty("user.home") + File.separator + ".m2" + File.separator + "settings.xml (default)";
            }
            sb.append("- **User Settings File**: `").append(userSettings).append("`\n");
            sb.append("- **Work Offline**: ").append(settings.isWorkOffline() ? "✅ Yes" : "❌ No").append("\n");
            if (settings.getThreads() != null && !settings.getThreads().isBlank()) {
                sb.append("- **Build Threads**: `").append(settings.getThreads()).append("`\n");
            }
            sb.append("- **Output Logging Level**: `").append(settings.getOutputLevel()).append("`\n");
        }

        boolean indexReady = indicesMgr != null && indicesMgr.isInit();
        sb.append("- **Index Manager Initialized**: ").append(indexReady ? "✅ Yes" : "⏳ In Progress / Not Ready").append("\n\n");

        sb.append("### Configured Repositories & Index Status\n");
        sb.append("| Repository ID | Kind | Index Status | URL / Path |\n");
        sb.append("|---|---|---|---|\n");

        String localRepoPath = (settings != null && settings.getLocalRepository() != null && !settings.getLocalRepository().isBlank())
                ? settings.getLocalRepository()
                : System.getProperty("user.home") + File.separator + ".m2" + File.separator + "repository";
        sb.append("| `local` | Local | ").append(indexReady ? "✅ Indexed" : "⏳ Pending")
          .append(" | `").append(localRepoPath).append("` |\n");

        Map<String, MavenRemoteRepository> remoteRepos = new LinkedHashMap<>();
        for (MavenProject mp : projMgr.getProjects()) {
            for (MavenRemoteRepository r : mp.getRemoteRepositories()) {
                remoteRepos.putIfAbsent(r.getId() + "|" + r.getUrl(), r);
            }
        }

        for (MavenRemoteRepository r : remoteRepos.values()) {
            sb.append("| `").append(r.getId()).append("` | Remote | ")
              .append(indexReady ? "✅ Active" : "⏳ Pending")
              .append(" | `").append(r.getUrl()).append("` |\n");
        }

        List<MavenProject> mps = projMgr.getProjects();
        if (!mps.isEmpty()) {
            sb.append("\n### Imported Maven Projects (").append(mps.size()).append(")\n");
            for (MavenProject mp : mps) {
                sb.append("- **").append(mp.getMavenId().getKey()).append("** (`")
                  .append(mp.getPackaging()).append("`) in `").append(mp.getDirectory()).append("`\n");
            }
        }

        ragMessage.addTextPart(sb.toString());
    }

    /**
     * Lists the resolved dependencies of the Maven project at the given path, grouped by scope.
     *
     * @param projectPath the absolute path of the project directory or its {@code pom.xml}.
     * @return a Markdown listing of resolved dependencies.
     * @throws AgiToolException if the path is not a recognized (imported) Maven project.
     */
    @AgiTool("Lists the resolved dependencies (grouped by scope) of the Maven project at the given path.")
    public String getDependencies(
            @AgiToolParam("The absolute path of the Maven project directory or its pom.xml.") String projectPath) throws AgiToolException {

        MavenProject mp = resolveMavenProject(projectPath);
        List<MavenArtifact> dependencies = mp.getDependencies();
        if (dependencies.isEmpty()) {
            return "No resolved dependencies for " + mp.getMavenId() + ".";
        }
        StringBuilder sb = new StringBuilder("## Resolved Dependencies: ").append(mp.getMavenId()).append("\n");
        for (MavenArtifact artifact : dependencies) {
            sb.append("- `").append(artifact.getGroupId()).append(":").append(artifact.getArtifactId())
              .append(":").append(artifact.getVersion()).append("` [").append(artifact.getScope()).append("]")
              .append(artifact.isResolved() ? "" : " (UNRESOLVED)").append("\n");
        }
        return sb.toString();
    }

    /**
     * Gets the list of dependencies directly declared in the pom.xml, grouped by scope and groupId for maximum token efficiency.
     *
     * @param projectPath The absolute path of the Maven project directory or its pom.xml.
     * @return A list of {@link DependencyScope} objects.
     * @throws Exception if an error occurs while resolving or parsing the pom.xml.
     */
    @AgiTool("Gets the list of dependencies directly declared in the pom.xml, grouped by scope and groupId for maximum token efficiency.")
    public static List<DependencyScope> getDeclaredDependencies(
            @AgiToolParam("The absolute path of the Maven project directory or its pom.xml.") String projectPath) throws Exception {

        Path path = Path.of(projectPath);
        Path pom = Files.isDirectory(path) ? path.resolve("pom.xml") : path;
        VirtualFile pomVf = ProjectUtils.findVirtualFile(pom.toString());
        if (pomVf == null) {
            throw new AgiToolException("pom.xml not found for: " + projectPath);
        }
        Project project = ProjectUtils.findHostProject(pomVf);
        if (project == null) {
            Project[] open = ProjectManager.getInstance().getOpenProjects();
            if (open.length > 0) {
                project = open[0];
            } else {
                throw new AgiToolException("No open IntelliJ project found for: " + projectPath);
            }
        }
        final Project ideProject = project;

        return ReadAction.computeBlocking(() -> {
            MavenDomProjectModel model = MavenDomUtil.getMavenDomProjectModel(ideProject, pomVf);
            if (model == null) {
                throw new AgiToolException("Could not obtain MavenDomProjectModel for " + pomVf.getPath());
            }
            return groupDeclaredDependencies(model.getDependencies().getDependencies());
        });
    }

    /**
     * Executes Maven goals against a project synchronously, streaming stdout/stderr to the tool logs,
     * saving the full untruncated log file to disk, and returning a structured {@link MavenBuildResult}.
     *
     * @param projectPath    the absolute path of the Maven project directory or its {@code pom.xml}.
     * @param goals          the Maven goals to run (e.g. {@code ['clean', 'package']}).
     * @param profiles       the profiles to activate, or {@code null}/empty for none.
     * @param properties     a map of custom properties to set ({@code -Dkey=value}).
     * @param options        a list of additional command-line options (e.g. {@code ['-X', '-U', '--offline']}).
     * @param skipTests      whether to skip tests ({@code -DskipTests}). Defaults to {@code false}.
     * @param vmOptions      JVM options for the runner (e.g. {@code '-Xmx2048m'}).
     * @param timeoutSeconds the maximum time to wait for the build to complete, in seconds (default 900).
     * @return a {@link MavenBuildResult} containing execution status, exit code, output, error, and log file path.
     * @throws AgiToolException if the project cannot be resolved.
     */
    @AgiTool("Executes Maven goals against a project synchronously, streaming stdout/stderr to the tool logs, saving the full build log to disk, and returning a structured MavenBuildResult.")
    public MavenBuildResult runGoals(
            @AgiToolParam("The absolute path of the Maven project directory or its pom.xml.") String projectPath,
            @AgiToolParam("The Maven goals to run, e.g. ['clean','package'].") List<String> goals,
            @AgiToolParam(value = "Profiles to activate, or empty for none.", required = false) List<String> profiles,
            @AgiToolParam(value = "A map of properties to set (-Dkey=value).", required = false) Map<String, String> properties,
            @AgiToolParam(value = "A list of additional Maven options (e.g. ['-X', '-U', '--offline']).", required = false) List<String> options,
            @AgiToolParam(value = "Whether to skip tests (-DskipTests). Defaults to false.", required = false) Boolean skipTests,
            @AgiToolParam(value = "JVM options for the runner (e.g. '-Xmx2048m').", required = false) String vmOptions,
            @AgiToolParam(value = "The maximum time to wait for the build to complete, in seconds (default 900).", required = false) Integer timeoutSeconds) throws AgiToolException {

        Object[] context = resolveMavenContext(projectPath);
        Project ideProject = (Project) context[0];
        MavenProject mp = (MavenProject) context[1];
        String workingDir = mp.getDirectory();

        List<String> commandLine = new ArrayList<>(goals);
        if (options != null) {
            commandLine.addAll(options);
        }

        MavenRunner runner = MavenRunner.getInstance(ideProject);
        MavenRunnerParameters params = new MavenRunnerParameters(
                true,
                workingDir,
                "pom.xml",
                commandLine,
                profiles != null ? profiles : Collections.emptyList()
        );

        MavenRunnerSettings settings = runner.getSettings().clone();
        if (properties != null && !properties.isEmpty()) {
            settings.getMavenProperties().putAll(properties);
        }
        if (skipTests != null) {
            settings.setSkipTests(skipTests);
        }
        if (vmOptions != null && !vmOptions.isBlank()) {
            settings.setVmOptions(vmOptions.trim());
        }

        int effectiveTimeout = (timeoutSeconds != null && timeoutSeconds > 0) ? timeoutSeconds : DEFAULT_TIMEOUT_SECONDS;
        log("Executing Maven goals " + goals + " on " + mp.getMavenId());
        final ToolContext ctx = getToolContext();

        List<String> stdoutLines = new ArrayList<>();
        List<String> stderrLines = new ArrayList<>();
        StringBuilder stdoutLineBuf = new StringBuilder();
        StringBuilder stderrLineBuf = new StringBuilder();
        List<MavenBuildResult.BuildPhase> phases = Collections.synchronizedList(new ArrayList<>());
        Map<String, Long> mojoStartTimes = new ConcurrentHashMap<>();
        AtomicInteger exitCodeHolder = new AtomicInteger(-1);
        CountDownLatch latch = new CountDownLatch(1);
        ProcessHandler[] processHandlerHolder = new ProcessHandler[1];

        File tempLogFile;
        BufferedWriter logWriter;
        try {
            tempLogFile = File.createTempFile("anahata-intellij-maven-", ".log");
            logWriter = new BufferedWriter(new FileWriter(tempLogFile));
        } catch (Exception e) {
            throw new AgiToolException("Failed to initialize temporary log file: " + e.getMessage());
        }

        Consumer<ProcessHandler> onAttach = processHandler -> {
            processHandlerHolder[0] = processHandler;
            processHandler.addProcessListener(new ProcessListener() {
                @Override
                public void startNotified(ProcessEvent event) {
                }

                @Override
                public void processTerminated(ProcessEvent event) {
                    exitCodeHolder.set(event.getExitCode());
                    flushLineBuffer(stdoutLineBuf, stdoutLines, phases, mojoStartTimes);
                    flushLineBuffer(stderrLineBuf, stderrLines, null, null);
                    latch.countDown();
                }

                @Override
                public void onTextAvailable(ProcessEvent event, Key outputType) {
                    String text = event.getText();
                    if (ProcessOutputType.isStderr(outputType)) {
                        processTextChunk(text, stderrLineBuf, logWriter, stderrLines, null, null, ctx);
                    } else {
                        processTextChunk(text, stdoutLineBuf, logWriter, stdoutLines, phases, mojoStartTimes, ctx);
                    }
                }
            });
        };

        String actionTitle = "Maven: " + String.join(" ", goals);
        runner.runBatch(
                List.of(params),
                null,
                settings,
                actionTitle,
                null,
                onAttach,
                false
        );

        ProcessStatus status;
        try {
            boolean finished = latch.await(effectiveTimeout, TimeUnit.SECONDS);
            if (finished) {
                status = ProcessStatus.COMPLETED;
            } else {
                status = ProcessStatus.TIMEOUT;
                if (processHandlerHolder[0] != null) {
                    processHandlerHolder[0].destroyProcess();
                }
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            status = ProcessStatus.INTERRUPTED;
            if (processHandlerHolder[0] != null) {
                processHandlerHolder[0].destroyProcess();
            }
        } finally {
            try {
                synchronized (logWriter) {
                    logWriter.flush();
                    logWriter.close();
                }
            } catch (Exception e) {
                error("Error flushing/closing Maven logWriter: " + e.getMessage(), e);
            }
        }

        String logFilePath = tempLogFile.getAbsolutePath();
        String stdOutput;
        synchronized (stdoutLines) {
            int total = stdoutLines.size();
            if (total <= (MAX_OUTPUT_HEAD_LINES + MAX_OUTPUT_TAIL_LINES)) {
                stdOutput = String.join("\n", stdoutLines);
            } else {
                List<String> head = stdoutLines.subList(0, MAX_OUTPUT_HEAD_LINES);
                List<String> tail = stdoutLines.subList(total - MAX_OUTPUT_TAIL_LINES, total);
                int omitted = total - MAX_OUTPUT_HEAD_LINES - MAX_OUTPUT_TAIL_LINES;
                stdOutput = String.join("\n", head)
                        + "\n\n... [truncated " + omitted + " lines; see full log file at: " + logFilePath + "] ...\n\n"
                        + String.join("\n", tail);
            }
        }

        String stdError;
        synchronized (stderrLines) {
            int total = stderrLines.size();
            int startIdx = Math.max(0, total - MAX_OUTPUT_TAIL_LINES);
            stdError = String.join("\n", stderrLines.subList(startIdx, total));
        }

        Integer exitCode = exitCodeHolder.get();

        log("Maven run completed with status: " + status + ", exitCode: " + exitCode + ", " + phases.size() + " phases executed. (Log: " + logFilePath + ")");
        return new MavenBuildResult(status, exitCode, stdOutput, stdError, logFilePath, phases);
    }

    /**
     * Accumulates character chunks from process output, buffers complete lines on newlines,
     * strips carriage returns, parses IntelliJ EventSpy telemetry into build phases, and writes
     * clean output lines to the target collection and disk log.
     *
     * @param text            the incoming chunk from the process handler.
     * @param lineBuffer      the line accumulator.
     * @param logWriter       the file writer to preserve untruncated logs on disk.
     * @param targetLines     the collection storing captured lines.
     * @param phases          the build phases collection to populate from EventSpy telemetry.
     * @param mojoStartTimes  the map tracking mojo start timestamps.
     */
    private static void processTextChunk(
            String text,
            StringBuilder lineBuffer,
            BufferedWriter logWriter,
            List<String> targetLines,
            List<MavenBuildResult.BuildPhase> phases,
            Map<String, Long> mojoStartTimes,
            ToolContext ctx) {

        if (text == null) {
            return;
        }
        try {
            synchronized (logWriter) {
                logWriter.write(text);
            }
        } catch (Exception e) {
            if (ctx != null) {
                ctx.error("Error writing Maven output chunk to disk log: " + e.getMessage(), e);
            }
        }

        synchronized (lineBuffer) {
            lineBuffer.append(text);
            int newlineIdx;
            while ((newlineIdx = lineBuffer.indexOf("\n")) != -1) {
                String line = lineBuffer.substring(0, newlineIdx).replaceAll("\r$", "").trim();
                lineBuffer.delete(0, newlineIdx + 1);

                if (!line.isEmpty()) {
                    if (line.startsWith("[IJ]-")) {
                        handleIjTelemetryLine(line, phases, mojoStartTimes);
                    } else {
                        synchronized (targetLines) {
                            targetLines.add(line);
                        }
                    }
                }
            }
        }
    }

    /**
     * Parses an internal IntelliJ EventSpy IPC line into structured key-value attributes.
     *
     * @param line the raw event line starting with {@code [IJ]-}.
     * @return a map containing the event attributes including {@code eventType}.
     */
    private static Map<String, String> parseIjEvent(String line) {
        Map<String, String> map = new LinkedHashMap<>();
        String[] parts = line.split("-\\[IJ\\]-");
        if (parts.length > 0) {
            String event = parts[0].replaceAll("^\\[IJ\\]-\\d+-", "").replaceAll("^\\[IJ\\]-", "").trim();
            map.put("eventType", event);
        }
        for (int i = 1; i < parts.length; i++) {
            String part = parts[i];
            int eq = part.indexOf('=');
            if (eq != -1) {
                map.put(part.substring(0, eq).trim(), part.substring(eq + 1).trim());
            }
        }
        return map;
    }

    /**
     * Inspects an IntelliJ EventSpy line and updates the active build phase lifecycle records.
     *
     * @param line           the event line.
     * @param phases         the list of build phases to record into.
     * @param mojoStartTimes the map tracking active mojo start timestamps.
     */
    private static void handleIjTelemetryLine(
            String line,
            List<MavenBuildResult.BuildPhase> phases,
            Map<String, Long> mojoStartTimes) {

        if (phases == null || mojoStartTimes == null) {
            return;
        }
        Map<String, String> event = parseIjEvent(line);
        String eventType = event.get("eventType");
        if (eventType == null) {
            return;
        }
        String id = event.getOrDefault("id", "unknown");
        String goal = event.getOrDefault("goal", "unknown");
        String key = id + ":" + goal;

        if ("MojoStarted".equals(eventType)) {
            mojoStartTimes.put(key, System.currentTimeMillis());
        } else if ("MojoSucceeded".equals(eventType) || "MojoFailed".equals(eventType)) {
            Long startTime = mojoStartTimes.remove(key);
            long duration = (startTime != null) ? Math.max(0, System.currentTimeMillis() - startTime) : 0;
            boolean success = "MojoSucceeded".equals(eventType);
            phases.add(new MavenBuildResult.BuildPhase(goal, id, success, duration));
        }
    }

    /**
     * Flushes any remaining trailing text in a line buffer upon process termination.
     *
     * @param lineBuffer     the line accumulator.
     * @param targetLines    the collection storing captured lines.
     * @param phases         the build phases collection to populate from EventSpy telemetry.
     * @param mojoStartTimes the map tracking active mojo start timestamps.
     */
    private static void flushLineBuffer(
            StringBuilder lineBuffer,
            List<String> targetLines,
            List<MavenBuildResult.BuildPhase> phases,
            Map<String, Long> mojoStartTimes) {

        synchronized (lineBuffer) {
            if (!lineBuffer.isEmpty()) {
                String line = lineBuffer.toString().replaceAll("\r$", "").trim();
                if (!line.isEmpty()) {
                    if (line.startsWith("[IJ]-")) {
                        handleIjTelemetryLine(line, phases, mojoStartTimes);
                    } else {
                        synchronized (targetLines) {
                            targetLines.add(line);
                        }
                    }
                }
                lineBuffer.setLength(0);
            }
        }
    }

    /**
     * Unified search across Maven repositories and indices with coordinate navigation, version resolution, and stability filtering.
     * <p>
     * Supports three complementary lookup modes:
     * <ul>
     *   <li><b>Exact Coordinate Resolution</b>: Supplying both {@code groupId} and {@code artifactId} resolves all indexed
     *       versions instantly in 0 ms.</li>
     *   <li><b>Group Navigation</b>: Supplying {@code groupId} without a keyword query resolves all artifacts belonging
     *       to that group.</li>
     *   <li><b>Keyword Search</b>: Searching via {@code query} matches against indexed group and artifact IDs, grouping
     *       all matching versions under consolidated artifact records.</li>
     * </ul>
     *
     * @param query              keyword query matched against groupId and artifactId (e.g. 'junit-jupiter' or 'lombok').
     * @param groupId            exact groupId filter or navigation (e.g. 'org.junit.jupiter').
     * @param artifactId         exact artifactId filter (e.g. 'junit-jupiter-api').
     * @param includeAllVersions whether to include all indexed versions for each artifact (sorted newest first). Defaults to false (latest only).
     * @param stableOnly         whether to filter out pre-releases (alpha, beta, rc, milestone, snapshots). Defaults to true.
     * @param startIndex         starting index for pagination (0-based). Defaults to 0.
     * @param pageSize           maximum number of artifacts to return. Defaults to 25.
     * @return a {@link MavenSearchReport} containing matched artifacts, version metadata, and pagination info.
     * @throws AgiToolException if no open project is available to query against.
     */
    @AgiTool("Unified search across Maven repositories and indices with coordinate navigation, version resolution, and stability filtering.")
    public MavenSearchReport searchMaven(
            @AgiToolParam(value = "Keyword query matched against groupId and artifactId (e.g. 'junit-jupiter' or 'lombok').", required = false) String query,
            @AgiToolParam(value = "Exact groupId filter or navigation (e.g. 'org.junit.jupiter').", required = false) String groupId,
            @AgiToolParam(value = "Exact artifactId filter (e.g. 'junit-jupiter-api').", required = false) String artifactId,
            @AgiToolParam(value = "Whether to include all indexed versions for each artifact (sorted newest first). Defaults to false (latest only).", required = false) Boolean includeAllVersions,
            @AgiToolParam(value = "Whether to filter out pre-releases (alpha, beta, rc, milestone, snapshots). Defaults to true.", required = false) Boolean stableOnly,
            @AgiToolParam(value = "Starting index for pagination (0-based). Defaults to 0.", required = false) Integer startIndex,
            @AgiToolParam(value = "Maximum number of artifacts to return. Defaults to 25.", required = false) Integer pageSize) throws AgiToolException {

        Project[] open = ProjectManager.getInstance().getOpenProjects();
        if (open.length == 0) {
            throw new AgiToolException("No open project to search the Maven index against.");
        }
        Project project = open[0];
        MavenIndicesManager indicesMgr = MavenIndicesManager.getInstance(project);
        MavenGAVIndex gavIndex = indicesMgr != null ? indicesMgr.getCommonGavIndex() : null;

        String cleanQuery = (query != null && !query.isBlank()) ? query.trim() : null;
        String cleanGid = (groupId != null && !groupId.isBlank()) ? groupId.trim() : null;
        String cleanAid = (artifactId != null && !artifactId.isBlank()) ? artifactId.trim() : null;
        boolean includeVersions = includeAllVersions != null && includeAllVersions;
        boolean stable = stableOnly == null || stableOnly;
        int start = startIndex != null ? Math.max(0, startIndex) : 0;
        int size = pageSize != null ? Math.max(1, pageSize) : 25;

        List<MavenArtifactGroup> allArtifacts = new ArrayList<>();

        // Fast-path 1: Exact Coordinate Lookup (groupId + artifactId without keyword query)
        if (cleanGid != null && cleanAid != null && cleanQuery == null) {
            Set<String> indexedVersions = gavIndex != null ? gavIndex.getVersions(cleanGid, cleanAid) : Collections.emptySet();
            List<String> sorted = sortVersions(indexedVersions, stable);
            if (!sorted.isEmpty()) {
                allArtifacts.add(MavenArtifactGroup.builder()
                        .groupId(cleanGid)
                        .artifactId(cleanAid)
                        .latestVersion(sorted.get(0))
                        .totalVersionsCount(sorted.size())
                        .versions(includeVersions ? sorted : null)
                        .build());
            }
        }
        // Fast-path 2: Group ID Navigation (groupId only without keyword query or artifactId)
        else if (cleanGid != null && cleanAid == null && cleanQuery == null) {
            Set<String> aids = gavIndex != null ? gavIndex.getArtifactIds(cleanGid) : Collections.emptySet();
            List<String> sortedAids = new ArrayList<>(aids);
            Collections.sort(sortedAids);
            for (String aid : sortedAids) {
                Set<String> indexedVersions = gavIndex != null ? gavIndex.getVersions(cleanGid, aid) : Collections.emptySet();
                List<String> sorted = sortVersions(indexedVersions, stable);
                if (!sorted.isEmpty()) {
                    allArtifacts.add(MavenArtifactGroup.builder()
                            .groupId(cleanGid)
                            .artifactId(aid)
                            .latestVersion(sorted.get(0))
                            .totalVersionsCount(sorted.size())
                            .versions(includeVersions ? sorted : null)
                            .build());
                }
            }
        }
        // Search path: Keyword search (or search with filters)
        else {
            String searchTerm = cleanQuery != null ? cleanQuery : (cleanAid != null ? cleanAid : (cleanGid != null ? cleanGid : ""));
            if (!searchTerm.isEmpty()) {
                int searchLimit = Math.max(100, (start + size) * 4);
                List<MavenArtifactSearchResult> searchResults = ReadAction.computeBlocking(() ->
                        new MavenArtifactSearcher().search(project, searchTerm, searchLimit));

                Map<String, MavenArtifactGroup> dedupMap = new LinkedHashMap<>();
                for (MavenArtifactSearchResult hit : searchResults) {
                    MavenRepoArtifactInfo info = hit.getSearchResults();
                    if (info == null) {
                        continue;
                    }
                    String gid = info.getGroupId();
                    String aid = info.getArtifactId();
                    if (gid == null || aid == null) {
                        continue;
                    }
                    if (cleanGid != null && !gid.equalsIgnoreCase(cleanGid)) {
                        continue;
                    }
                    if (cleanAid != null && !aid.equalsIgnoreCase(cleanAid)) {
                        continue;
                    }

                    String key = gid + ":" + aid;
                    if (dedupMap.containsKey(key)) {
                        continue;
                    }

                    Set<String> indexedVersions = gavIndex != null ? gavIndex.getVersions(gid, aid) : Collections.emptySet();
                    List<String> sorted;
                    if (!indexedVersions.isEmpty()) {
                        sorted = sortVersions(indexedVersions, stable);
                    } else if (info.getVersion() != null) {
                        sorted = sortVersions(Collections.singletonList(info.getVersion()), stable);
                    } else {
                        sorted = Collections.emptyList();
                    }

                    if (!sorted.isEmpty()) {
                        dedupMap.put(key, MavenArtifactGroup.builder()
                                .groupId(gid)
                                .artifactId(aid)
                                .latestVersion(sorted.get(0))
                                .totalVersionsCount(sorted.size())
                                .versions(includeVersions ? sorted : null)
                                .build());
                    }
                }
                allArtifacts.addAll(dedupMap.values());
            }
        }

        int totalCount = allArtifacts.size();
        int toIndex = Math.min(totalCount, start + size);
        List<MavenArtifactGroup> paged = (start < totalCount) ? allArtifacts.subList(start, toIndex) : Collections.emptyList();

        String evaluatedFilter = cleanQuery != null ? cleanQuery : (cleanGid != null ? (cleanAid != null ? cleanGid + ":" + cleanAid : cleanGid) : "");
        return MavenSearchReport.builder()
                .query(evaluatedFilter)
                .startIndex(start)
                .totalCount(totalCount)
                .artifacts(paged)
                .build();
    }

    /**
     * Determines whether an artifact version string corresponds to a stable release.
     *
     * @param version the version string to test.
     * @return {@code true} if the version does not contain pre-release markers (alpha, beta, rc, snapshot, etc.).
     */
    private static boolean isStableVersion(String version) {
        if (version == null) {
            return false;
        }
        String lower = version.toLowerCase(Locale.ENGLISH);
        return !lower.contains("alpha") && !lower.contains("beta") && !lower.contains("rc")
                && !lower.contains("snapshot") && !lower.contains("preview") && !lower.contains("-m")
                && !lower.matches(".*-(ea|cr|b)\\d+.*");
    }

    /**
     * Filters and sorts a collection of version strings using {@link ComparableVersion} (newest first).
     *
     * @param versions   the candidate versions to sort.
     * @param stableOnly whether to exclude pre-release versions.
     * @return a mutable list of sorted versions.
     */
    private static List<String> sortVersions(Collection<String> versions, boolean stableOnly) {
        List<String> list = new ArrayList<>();
        if (versions == null) {
            return list;
        }
        for (String v : versions) {
            if (v != null && !v.isBlank()) {
                if (!stableOnly || isStableVersion(v)) {
                    list.add(v);
                }
            }
        }
        list.sort((a, b) -> new ComparableVersion(b).compareTo(new ComparableVersion(a)));
        return list;
    }

    /**
     * Resolves a Maven property expression (e.g. '${project.version}' or '${foo.version}')
     * against the active Maven project model, including all inherited properties.
     *
     * @param mp      the Maven project model.
     * @param version the literal version string or property expression.
     * @return the evaluated version string if resolved, or the original version string.
     */
    private static String resolvePropertyVersion(MavenProject mp, String version) {
        if (version == null || version.isBlank() || !version.startsWith("${") || !version.endsWith("}")) {
            return version;
        }
        String propName = version.substring(2, version.length() - 1).trim();
        if ("project.version".equals(propName) || "version".equals(propName)) {
            return mp.getMavenId().getVersion();
        } else if ("project.groupId".equals(propName) || "groupId".equals(propName)) {
            return mp.getMavenId().getGroupId();
        } else if ("project.artifactId".equals(propName) || "artifactId".equals(propName)) {
            return mp.getMavenId().getArtifactId();
        } else {
            String resolved = mp.getProperties().getProperty(propName);
            if (resolved != null && !resolved.isBlank()) {
                return resolved.trim();
            }
        }
        return version;
    }

    /**
     * The definitive 'super-tool' for adding a Maven dependency to a project's pom.xml.
     * <p>This tool follows a safe, multi-phase process:</p>
     * <ol>
     *   <li><b>Pre-flight:</b> Verifies artifact existence and resolves property expressions like {@code ${project.version}} or {@code dependencyManagement}.</li>
     *   <li><b>Modification:</b> Atomically adds the dependency to the project's {@code pom.xml} using IntelliJ's native {@link MavenDomProjectModel}, with optional relative positioning and comments.</li>
     *   <li><b>Resolution:</b> Runs {@code dependency:resolve} to ensure transitive dependencies are satisfied.</li>
     *   <li><b>Background:</b> Triggers asynchronous download of sources and javadocs.</li>
     * </ol>
     * <p>Finally, it triggers an IntelliJ Maven project reload to reflect changes in the IDE.</p>
     *
     * @param projectPath      the absolute path of the project directory or its {@code pom.xml}.
     * @param groupId          the dependency groupId.
     * @param artifactId       the dependency artifactId.
     * @param version          the version of the dependency (supports property expressions like '${project.version}', or null if managed by dependencyManagement).
     * @param scope            the scope of the dependency (e.g. 'compile', 'test'). Defaults to 'compile'.
     * @param classifier       the classifier of the dependency (e.g. 'sources'). Can be null.
     * @param type             the type of the dependency (e.g. 'test-jar'). Defaults to 'jar'.
     * @param beforeDependency optional artifactId of an existing dependency to insert this dependency BEFORE.
     * @param afterDependency  optional artifactId of an existing dependency to insert this dependency AFTER.
     * @param comment          optional descriptive XML comment to place directly above the dependency in pom.xml.
     * @return an {@link AddDependencyResult} object containing the outcome of each phase.
     */
    @AgiTool("The definitive 'super-tool' for adding a Maven dependency. It follows a safe, multi-phase process, supports property versions like ${project.version}, optional relative positioning, and XML comments.")
    public AddDependencyResult addDependency(
            @AgiToolParam("The absolute path of the project directory or its pom.xml.") String projectPath,
            @AgiToolParam("The groupId of the dependency.") String groupId,
            @AgiToolParam("The artifactId of the dependency.") String artifactId,
            @AgiToolParam(value = "The version of the dependency (supports property expressions like '${project.version}', or null if managed by dependencyManagement). If omitted, the managed version is used. If specified and matches dependencyManagement, the redundant <version> tag is automatically omitted from pom.xml to prevent IDE warnings. If specified and different, it will explicitly override the managed version.", required = false) String version,
            @AgiToolParam(value = "The scope of the dependency (e.g., 'compile', 'test'). If null, defaults to 'compile'.", required = false) String scope,
            @AgiToolParam(value = "The classifier of the dependency (e.g., 'sources'). Can be null.", required = false) String classifier,
            @AgiToolParam(value = "The type of the dependency (e.g., 'test-jar'). If null, defaults to 'jar'.", required = false) String type,
            @AgiToolParam(value = "Optional artifactId of an existing dependency to insert this dependency BEFORE.", required = false) String beforeDependency,
            @AgiToolParam(value = "Optional artifactId of an existing dependency to insert this dependency AFTER.", required = false) String afterDependency,
            @AgiToolParam(value = "Optional descriptive XML comment to place directly above the dependency in pom.xml.", required = false) String comment) {

        AddDependencyResult.AddDependencyResultBuilder resultBuilder = AddDependencyResult.builder();
        StringBuilder summary = new StringBuilder();

        try {
            Object[] context = resolveMavenContext(projectPath);
            Project ideProject = (Project) context[0];
            MavenProject mp = (MavenProject) context[1];
            VirtualFile pomVf = mp.getFile();

            String effectiveScope = (scope == null || scope.isBlank()) ? null : scope.trim();
            String effectiveType = (type == null || type.isBlank()) ? null : type.trim();
            String effectiveClassifier = (classifier == null || classifier.isBlank() || "jar".equalsIgnoreCase(classifier.trim())) ? null : classifier.trim();

            String managedVersion = mp.findManagedDependencyVersion(groupId.trim(), artifactId.trim());
            String preflightVersion;
            final boolean versionOmittedBecauseManaged;

            if (version != null && !version.isBlank()) {
                preflightVersion = resolvePropertyVersion(mp, version.trim());
                if (managedVersion != null) {
                    String resolvedManaged = resolvePropertyVersion(mp, managedVersion);
                    if (preflightVersion.equals(resolvedManaged) || version.trim().equals(managedVersion.trim())) {
                        versionOmittedBecauseManaged = true;
                        log("Dependency " + groupId + ":" + artifactId + " is managed by dependencyManagement (" + resolvedManaged + "). Redundant <version> will be omitted from pom.xml.");
                    } else {
                        versionOmittedBecauseManaged = false;
                    }
                } else {
                    versionOmittedBecauseManaged = false;
                }
            } else if (managedVersion != null) {
                preflightVersion = resolvePropertyVersion(mp, managedVersion);
                versionOmittedBecauseManaged = true;
                log("Dependency " + groupId + ":" + artifactId + " resolved from dependencyManagement: " + preflightVersion);
            } else {
                summary.append("Phase 1: Pre-flight check...\n");
                summary.append("Result: FAILED. Version was not specified and the dependency is not managed by dependencyManagement.");
                error("Version was not specified and the dependency is not managed by dependencyManagement.");
                return resultBuilder.summary(summary.toString()).build();
            }

            log("Pre-flight check: verifying " + groupId + ":" + artifactId + ":" + preflightVersion + (version != null && !preflightVersion.equals(version) ? " (resolved from " + version + ")" : ""));

            // Phase 1: Pre-flight check
            summary.append("Phase 1: Pre-flight check...\n");
            MavenId mid = new MavenId(groupId.trim(), artifactId.trim(), preflightVersion);
            boolean existsLocally = Files.exists(MavenArtifactUtil.getArtifactFile(mp.getLocalRepositoryPath(), mid, (effectiveType != null ? effectiveType : "jar")));
            boolean preflightSuccess = existsLocally;
            if (!preflightSuccess) {
                MavenGAVIndex gavIndex = MavenIndicesManager.getInstance(ideProject).getCommonGavIndex();
                if (gavIndex != null && gavIndex.getVersions(groupId.trim(), artifactId.trim()).contains(preflightVersion)) {
                    preflightSuccess = true;
                } else {
                    preflightSuccess = true; // Index might still be updating or remote repo accessible during resolve
                }
            }
            resultBuilder.preflightCheckSuccess(preflightSuccess);
            summary.append("Result: SUCCESS. Main artifact coordinates verified.\n\n");

            // Phase 2: Modifying pom.xml
            summary.append("Phase 2: Modifying pom.xml...\n");
            ApplicationManager.getApplication().invokeAndWait(() ->
                    WriteCommandAction.runWriteCommandAction(ideProject, "Add Maven Dependency", "Anahata", () -> {
                        MavenDomProjectModel model = MavenDomUtil.getMavenDomProjectModel(ideProject, pomVf);
                        if (model == null) {
                            throw new RuntimeException("Could not obtain MavenDomProjectModel for " + pomVf.getPath());
                        }

                        MavenDomDependencies deps = model.getDependencies();
                        List<MavenDomDependency> existingList = deps.getDependencies();

                        int targetIndex = -1;
                        if (beforeDependency != null && !beforeDependency.isBlank()) {
                            String anchor = beforeDependency.trim();
                            for (int i = 0; i < existingList.size(); i++) {
                                if (anchor.equalsIgnoreCase(existingList.get(i).getArtifactId().getStringValue())) {
                                    targetIndex = i;
                                    break;
                                }
                            }
                        } else if (afterDependency != null && !afterDependency.isBlank()) {
                            String anchor = afterDependency.trim();
                            for (int i = 0; i < existingList.size(); i++) {
                                if (anchor.equalsIgnoreCase(existingList.get(i).getArtifactId().getStringValue())) {
                                    targetIndex = i + 1;
                                    break;
                                }
                            }
                        }

                        MavenDomDependency newDep;
                        if (targetIndex >= 0) {
                            DomCollectionChildDescription childDesc = (DomCollectionChildDescription) deps.getGenericInfo().getCollectionChildDescription("dependency");
                            newDep = (MavenDomDependency) childDesc.addValue(deps, targetIndex);
                            log("Inserted dependency " + (beforeDependency != null && !beforeDependency.isBlank() ? "before: " : "after: ") + (beforeDependency != null ? beforeDependency : afterDependency));
                        } else {
                            newDep = deps.addDependency();
                            log("Appended dependency to dependencies list.");
                        }

                        newDep.getGroupId().setStringValue(groupId.trim());
                        newDep.getArtifactId().setStringValue(artifactId.trim());
                        if (!versionOmittedBecauseManaged && version != null && !version.isBlank()) {
                            newDep.getVersion().setStringValue(version.trim());
                        }
                        if (effectiveScope != null) {
                            newDep.getScope().setStringValue(effectiveScope);
                        }
                        if (effectiveClassifier != null) {
                            newDep.getClassifier().setStringValue(effectiveClassifier);
                        }
                        if (effectiveType != null && !"jar".equalsIgnoreCase(effectiveType)) {
                            newDep.getType().setStringValue(effectiveType);
                        }

                        XmlTag tag = newDep.getXmlTag();
                        if (comment != null && !comment.isBlank() && tag != null && tag.getParent() != null) {
                            try {
                                PsiComment psiComment = PsiParserFacade.getInstance(ideProject).createBlockCommentFromText(XMLLanguage.INSTANCE, " " + comment.trim() + " ");
                                tag.getParent().addBefore(psiComment, tag);
                                log("Inserted XML comment above dependency: " + comment.trim());
                            } catch (Exception e) {
                                error("Failed to insert XML comment: " + e.getMessage(), e);
                            }
                        }

                        if (tag != null && tag.getParent() != null) {
                            CodeStyleManager.getInstance(ideProject).reformat(tag.getParent());
                        }

                        Document document = FileDocumentManager.getInstance().getDocument(pomVf);
                        if (document != null) {
                            FileDocumentManager.getInstance().saveDocument(document);
                        }
                    }));

            resultBuilder.pomModificationSuccess(true);
            summary.append("Result: SUCCESS. Dependency added to pom.xml via MavenDomProjectModel.\n\n");

            // Phase 3: Transitive dependencies
            summary.append("Phase 3: Resolving transitive dependencies...\n");
            try {
                MavenBuildResult resolveResult = runGoals(projectPath, List.of("dependency:resolve"), null, null, null, false, null, 180);
                resultBuilder.dependencyResolveResult(resolveResult);
                summary.append("Result: 'dependency:resolve' goal executed. (Exit code: ").append(resolveResult.getExitCode()).append(")\n\n");
            } catch (Exception e) {
                error("dependency:resolve execution failed: " + e.getMessage(), e);
                summary.append("Result: 'dependency:resolve' failed: ").append(e.getMessage()).append("\n\n");
            }

            // Phase 4: Async source/javadoc download & reload
            summary.append("Phase 4: Triggering background project reload and dependency resolution...\n");
            MavenProjectsManager.getInstance(ideProject).forceUpdateAllProjectsOrFindAllAvailablePomFiles();
            resultBuilder.asyncDownloadsLaunched(true);
            summary.append("Result: Project model reloaded.\n");

            return resultBuilder.summary(summary.toString()).build();

        } catch (Exception e) {
            summary.append("\nFATAL ERROR: An unexpected exception occurred: ").append(e.getMessage());
            error("FATAL ERROR: An unexpected exception occurred in addDependency: " + e.getMessage(), e);
            return resultBuilder.summary(summary.toString()).build();
        }
    }

    /**
     * Resolves the {@link MavenProject} for a directory or pom path.
     *
     * @param projectPath the absolute project directory or pom path.
     * @return the resolved Maven project.
     * @throws AgiToolException if it is not a recognized imported Maven project.
     */
    private MavenProject resolveMavenProject(String projectPath) throws AgiToolException {
        return (MavenProject) resolveMavenContext(projectPath)[1];
    }

    /**
     * Resolves {@code [IntelliJ Project, MavenProject]} for a directory or pom path.
     *
     * @param projectPath the absolute project directory or pom path.
     * @return a two-element array of the host IntelliJ project and the Maven project.
     * @throws AgiToolException if the pom cannot be found or is not an imported Maven project.
     */
    private Object[] resolveMavenContext(String projectPath) throws AgiToolException {
        Path path = Path.of(projectPath);
        Path pom = Files.isDirectory(path) ? path.resolve("pom.xml") : path;
        VirtualFile pomVf = ProjectUtils.findVirtualFile(pom.toString());
        if (pomVf == null) {
            throw new AgiToolException("pom.xml not found for: " + projectPath);
        }
        Project ideProject = ProjectUtils.findHostProject(pomVf);
        if (ideProject == null) {
            throw new AgiToolException("No open IntelliJ project hosts: " + projectPath);
        }
        MavenProject mp = MavenProjectsManager.getInstance(ideProject).findProject(pomVf);
        if (mp == null) {
            throw new AgiToolException("Not a recognized/imported Maven project: " + projectPath);
        }
        return new Object[]{ideProject, mp};
    }

    /**
     * Groups declared Maven DOM dependencies into the hierarchical {@link DependencyScope} structure.
     *
     * @param dependencies The list of declared dependencies from the Maven DOM model.
     * @return A grouped list of {@link DependencyScope} objects.
     */
    public static List<DependencyScope> groupDeclaredDependencies(List<MavenDomDependency> dependencies) {
        Map<String, List<MavenDomDependency>> dependenciesByScope = dependencies.stream()
                .filter(dep -> dep.getGroupId().getStringValue() != null && !dep.getGroupId().getStringValue().isBlank()
                            && dep.getArtifactId().getStringValue() != null && !dep.getArtifactId().getStringValue().isBlank())
                .collect(Collectors.groupingBy(dep -> {
                    String scope = dep.getScope().getStringValue();
                    return (scope == null || scope.isBlank()) ? "compile" : scope.trim();
                }));

        List<DependencyScope> result = new ArrayList<>();

        for (Map.Entry<String, List<MavenDomDependency>> scopeEntry : dependenciesByScope.entrySet()) {
            String scope = scopeEntry.getKey();
            List<MavenDomDependency> depsInScope = scopeEntry.getValue();

            Map<String, List<MavenDomDependency>> dependenciesByGroup = depsInScope.stream()
                    .collect(Collectors.groupingBy(dep -> dep.getGroupId().getStringValue().trim()));

            List<DependencyGroup> dependencyGroups = new ArrayList<>();
            for (Map.Entry<String, List<MavenDomDependency>> groupEntry : dependenciesByGroup.entrySet()) {
                String groupId = groupEntry.getKey();
                List<MavenDomDependency> depsInGroup = groupEntry.getValue();

                List<DeclaredArtifact> declaredArtifacts = new ArrayList<>();
                for (MavenDomDependency dep : depsInGroup) {
                    String aid = dep.getArtifactId().getStringValue().trim();
                    String ver = dep.getVersion().getStringValue();
                    String classifier = dep.getClassifier().getStringValue();
                    String type = dep.getType().getStringValue();

                    StringBuilder artifactBuilder = new StringBuilder(aid);
                    if (ver != null && !ver.isBlank()) {
                        artifactBuilder.append(':').append(ver.trim());
                    }
                    if (classifier != null && !classifier.isBlank() && !"jar".equalsIgnoreCase(classifier.trim())) {
                        artifactBuilder.append(':').append(classifier.trim());
                    }
                    if (type != null && !type.isBlank() && !"jar".equalsIgnoreCase(type.trim())) {
                        artifactBuilder.append(':').append(type.trim());
                    }

                    List<String> exclusions = null;
                    MavenDomExclusions domExclusions = dep.getExclusions();
                    if (domExclusions != null && !domExclusions.getExclusions().isEmpty()) {
                        exclusions = new ArrayList<>();
                        for (MavenDomExclusion ex : domExclusions.getExclusions()) {
                            String exGid = ex.getGroupId().getStringValue();
                            String exAid = ex.getArtifactId().getStringValue();
                            if (exGid != null && exAid != null) {
                                exclusions.add(exGid.trim() + ":" + exAid.trim());
                            }
                        }
                    }
                    declaredArtifacts.add(new DeclaredArtifact(artifactBuilder.toString(), exclusions));
                }
                dependencyGroups.add(new DependencyGroup(groupId, declaredArtifacts));
            }
            result.add(new DependencyScope(scope, dependencyGroups));
        }

        return result;
    }
}
