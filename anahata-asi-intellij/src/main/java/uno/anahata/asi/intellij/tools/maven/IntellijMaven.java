/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.maven;

import com.intellij.execution.process.ProcessEvent;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.process.ProcessListener;
import com.intellij.execution.process.ProcessOutputType;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.Consumer;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.idea.maven.execution.MavenRunner;
import org.jetbrains.idea.maven.execution.MavenRunnerParameters;
import org.jetbrains.idea.maven.execution.MavenRunnerSettings;
import org.jetbrains.idea.maven.indices.MavenArtifactSearchResult;
import org.jetbrains.idea.maven.indices.MavenArtifactSearcher;
import org.jetbrains.idea.maven.model.MavenArtifact;
import org.jetbrains.idea.maven.project.MavenProject;
import org.jetbrains.idea.maven.project.MavenProjectsManager;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.ToolContext;
import uno.anahata.asi.intellij.internal.ProjectUtils;
import uno.anahata.asi.toolkit.maven.DeclaredArtifact;
import uno.anahata.asi.toolkit.maven.DependencyGroup;
import uno.anahata.asi.toolkit.maven.DependencyScope;
import uno.anahata.asi.toolkit.maven.MavenBuildResult;
import uno.anahata.asi.toolkit.maven.MavenBuildResult.ProcessStatus;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.InputStream;
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
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamReader;
import org.apache.maven.artifact.versioning.ComparableVersion;
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
     * @throws AgiToolException if an error occurs while parsing the pom.xml.
     */
    @AgiTool("Gets the list of dependencies directly declared in the pom.xml, grouped by scope and groupId for maximum token efficiency.")
    public static List<DependencyScope> getDeclaredDependencies(
            @AgiToolParam("The absolute path of the Maven project directory or its pom.xml.") String projectPath) throws AgiToolException {

        Path path = Path.of(projectPath);
        Path pom = Files.isDirectory(path) ? path.resolve("pom.xml") : path;
        try {
            return parseDeclaredDependencies(pom);
        } catch (Exception e) {
            log.error("Failed to parse declared dependencies for: " + projectPath, e);
            throw new AgiToolException("Failed to parse declared dependencies: " + e.getMessage());
        }
    }

    /** Maximum number of output lines to keep in stdOutput in the DTO (head + tail). */
    private static final int MAX_OUTPUT_HEAD_LINES = 25;
    private static final int MAX_OUTPUT_TAIL_LINES = 75;
    /** Default timeout for Maven build execution (15 minutes). */
    private static final int DEFAULT_TIMEOUT_SECONDS = 900;

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
                        processTextChunk(text, stderrLineBuf, logWriter, stderrLines, null, null);
                    } else {
                        processTextChunk(text, stdoutLineBuf, logWriter, stdoutLines, phases, mojoStartTimes);
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
            } catch (Exception ignored) {
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
            Map<String, Long> mojoStartTimes) {

        if (text == null) {
            return;
        }
        try {
            synchronized (logWriter) {
                logWriter.write(text);
            }
        } catch (Exception ignored) {
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
     * Adds a dependency to a project's {@code pom.xml} and triggers a Maven reimport.
     * <p>
     * The dependency element is spliced into the existing {@code <dependencies>} block (or a
     * new block is created before {@code </project>}) via a single undoable document edit,
     * the file is saved, and {@link MavenProjectsManager#forceUpdateAllProjectsOrFindAllAvailablePomFiles()}
     * refreshes the project model so the new artifact is resolved onto the classpath.
     * </p>
     *
     * @param projectPath the absolute path of the project directory or its {@code pom.xml}.
     * @param groupId     the dependency groupId.
     * @param artifactId  the dependency artifactId.
     * @param version     the dependency version.
     * @param scope       the Maven scope (e.g. {@code compile}, {@code test}), or {@code null} for default.
     * @return a confirmation message.
     * @throws AgiToolException if the pom cannot be resolved or edited.
     */
    @AgiTool("Adds a dependency to a project's pom.xml and triggers a Maven reimport.")
    public String addDependency(
            @AgiToolParam("The absolute path of the Maven project directory or its pom.xml.") String projectPath,
            @AgiToolParam("The dependency groupId.") String groupId,
            @AgiToolParam("The dependency artifactId.") String artifactId,
            @AgiToolParam("The dependency version.") String version,
            @AgiToolParam(value = "The Maven scope (compile/test/provided/runtime), or null for default.", required = false) String scope) throws AgiToolException {

        Object[] context = resolveMavenContext(projectPath);
        Project ideProject = (Project) context[0];
        MavenProject mp = (MavenProject) context[1];
        VirtualFile pomVf = mp.getFile();

        StringBuilder dep = new StringBuilder();
        dep.append("        <dependency>\n");
        dep.append("            <groupId>").append(groupId).append("</groupId>\n");
        dep.append("            <artifactId>").append(artifactId).append("</artifactId>\n");
        dep.append("            <version>").append(version).append("</version>\n");
        if (scope != null && !scope.isBlank()) {
            dep.append("            <scope>").append(scope).append("</scope>\n");
        }
        dep.append("        </dependency>\n");

        ApplicationManager.getApplication().invokeAndWait(() ->
                WriteCommandAction.runWriteCommandAction(ideProject, () -> {
                    Document document = FileDocumentManager.getInstance().getDocument(pomVf);
                    if (document == null) {
                        return;
                    }
                    String text = document.getText();
                    int closeDeps = text.lastIndexOf("</dependencies>");
                    if (closeDeps >= 0) {
                        document.insertString(closeDeps, dep.toString());
                    } else {
                        int closeProject = text.lastIndexOf("</project>");
                        String block = "    <dependencies>\n" + dep + "    </dependencies>\n";
                        document.insertString(closeProject >= 0 ? closeProject : text.length(), block);
                    }
                    FileDocumentManager.getInstance().saveDocument(document);
                }));

        MavenProjectsManager.getInstance(ideProject).forceUpdateAllProjectsOrFindAllAvailablePomFiles();
        log("Added dependency " + groupId + ":" + artifactId + ":" + version + " and triggered reimport.");
        return "Added " + groupId + ":" + artifactId + ":" + version + " to " + mp.getMavenId() + " and triggered a Maven reimport.";
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
     * Internal record holding raw declared dependency coordinates before scope grouping.
     */
    private record RawDependency(
            String groupId,
            String artifactId,
            String version,
            String scope,
            String classifier,
            String type,
            List<String> exclusions) {}

    /**
     * Parses the declared dependencies directly from a pom.xml file using standard XML stream parsing.
     *
     * @param pomPath The path to the pom.xml file.
     * @return A list of {@link DependencyScope} instances.
     * @throws Exception if XML reading or parsing fails.
     */
    public static List<DependencyScope> parseDeclaredDependencies(Path pomPath) throws Exception {
        if (!Files.exists(pomPath)) {
            return Collections.emptyList();
        }
        XMLInputFactory factory = XMLInputFactory.newInstance();
        factory.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, Boolean.FALSE);
        factory.setProperty(XMLInputFactory.SUPPORT_DTD, Boolean.FALSE);

        List<RawDependency> rawDeps = new ArrayList<>();
        try (InputStream is = Files.newInputStream(pomPath)) {
            XMLStreamReader reader = factory.createXMLStreamReader(is);
            int depth = 0;
            boolean inProjectDependencies = false;
            boolean inDependency = false;
            boolean inExclusions = false;
            boolean inExclusion = false;

            String currentTag = "";
            String groupId = "";
            String artifactId = "";
            String version = "";
            String scope = "compile";
            String classifier = null;
            String type = "jar";
            List<String> exclusions = new ArrayList<>();
            String exclGroupId = "";
            String exclArtifactId = "";

            while (reader.hasNext()) {
                int event = reader.next();
                switch (event) {
                    case XMLStreamConstants.START_ELEMENT -> {
                        depth++;
                        currentTag = reader.getLocalName();
                        if (depth == 2 && "dependencies".equals(currentTag)) {
                            inProjectDependencies = true;
                        } else if (inProjectDependencies && depth == 3 && "dependency".equals(currentTag)) {
                            inDependency = true;
                            groupId = "";
                            artifactId = "";
                            version = "";
                            scope = "compile";
                            classifier = null;
                            type = "jar";
                            exclusions = new ArrayList<>();
                        } else if (inDependency && depth == 4 && "exclusions".equals(currentTag)) {
                            inExclusions = true;
                        } else if (inExclusions && depth == 5 && "exclusion".equals(currentTag)) {
                            inExclusion = true;
                            exclGroupId = "";
                            exclArtifactId = "";
                        }
                    }
                    case XMLStreamConstants.CHARACTERS -> {
                        String text = reader.getText();
                        if (text == null || text.isBlank()) {
                            break;
                        }
                        text = text.trim();
                        if (inExclusion) {
                            if ("groupId".equals(currentTag)) {
                                exclGroupId += text;
                            } else if ("artifactId".equals(currentTag)) {
                                exclArtifactId += text;
                            }
                        } else if (inDependency && !inExclusions) {
                            switch (currentTag) {
                                case "groupId" -> groupId += text;
                                case "artifactId" -> artifactId += text;
                                case "version" -> version += text;
                                case "scope" -> scope = text;
                                case "classifier" -> classifier = text;
                                case "type" -> type = text;
                            }
                        }
                    }
                    case XMLStreamConstants.END_ELEMENT -> {
                        String endTag = reader.getLocalName();
                        if (inExclusion && "exclusion".equals(endTag)) {
                            if (!exclGroupId.isEmpty() || !exclArtifactId.isEmpty()) {
                                exclusions.add(exclGroupId + ":" + exclArtifactId);
                            }
                            inExclusion = false;
                        } else if (inExclusions && "exclusions".equals(endTag)) {
                            inExclusions = false;
                        } else if (inDependency && "dependency".equals(endTag)) {
                            if (!groupId.isEmpty() && !artifactId.isEmpty()) {
                                rawDeps.add(new RawDependency(
                                        groupId,
                                        artifactId,
                                        version,
                                        scope.isBlank() ? "compile" : scope,
                                        classifier,
                                        type,
                                        exclusions.isEmpty() ? null : exclusions
                                ));
                            }
                            inDependency = false;
                        } else if (inProjectDependencies && "dependencies".equals(endTag) && depth == 2) {
                            inProjectDependencies = false;
                        }
                        currentTag = "";
                        depth--;
                    }
                }
            }
        }
        return groupDeclaredDependencies(rawDeps);
    }

    /**
     * Groups raw declared dependencies into the hierarchical {@link DependencyScope} structure.
     *
     * @param dependencies The list of raw parsed dependencies.
     * @return A grouped list of {@link DependencyScope} objects.
     */
    public static List<DependencyScope> groupDeclaredDependencies(List<RawDependency> dependencies) {
        Map<String, List<RawDependency>> dependenciesByScope = dependencies.stream()
                .collect(Collectors.groupingBy(dep -> dep.scope() == null ? "compile" : dep.scope()));

        List<DependencyScope> result = new ArrayList<>();

        for (Map.Entry<String, List<RawDependency>> scopeEntry : dependenciesByScope.entrySet()) {
            String scope = scopeEntry.getKey();
            List<RawDependency> depsInScope = scopeEntry.getValue();

            Map<String, List<RawDependency>> dependenciesByGroup = depsInScope.stream()
                    .collect(Collectors.groupingBy(RawDependency::groupId));

            List<DependencyGroup> dependencyGroups = new ArrayList<>();
            for (Map.Entry<String, List<RawDependency>> groupEntry : dependenciesByGroup.entrySet()) {
                String groupId = groupEntry.getKey();
                List<RawDependency> depsInGroup = groupEntry.getValue();

                List<DeclaredArtifact> declaredArtifacts = new ArrayList<>();
                for (RawDependency dep : depsInGroup) {
                    StringBuilder artifactBuilder = new StringBuilder();
                    artifactBuilder.append(dep.artifactId());
                    if (!dep.version().isEmpty()) {
                        artifactBuilder.append(':').append(dep.version());
                    }
                    if (dep.classifier() != null && !dep.classifier().isEmpty()) {
                        artifactBuilder.append(':').append(dep.classifier());
                    }
                    if (dep.type() != null && !dep.type().equals("jar")) {
                        artifactBuilder.append(':').append(dep.type());
                    }

                    declaredArtifacts.add(new DeclaredArtifact(artifactBuilder.toString(), dep.exclusions()));
                }
                dependencyGroups.add(new DependencyGroup(groupId, declaredArtifacts));
            }
            result.add(new DependencyScope(scope, dependencyGroups));
        }

        return result;
    }
}
