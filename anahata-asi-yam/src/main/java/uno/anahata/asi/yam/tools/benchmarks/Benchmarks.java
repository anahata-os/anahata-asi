/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.benchmarks;

import java.awt.GraphicsEnvironment;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.AgiUserMessage;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.message.ResponseUsageMetadata;
import uno.anahata.asi.agi.provider.Response;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.ToolContext;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.yam.tools.screenrecording.ScreenRecordingOverlay;
import uno.anahata.asi.yam.tools.screenrecording.ScreenRecorder;
import uno.anahata.asi.yam.tools.screenrecording.RecordedSession;
import uno.anahata.asi.yam.tools.youtube.YouTube;
import uno.anahata.asi.yam.tools.youtube.YouTubeCredentials;
import uno.anahata.asi.yam.tools.youtube.YouTubeVideoUploadRequest;
import org.apache.commons.lang3.exception.ExceptionUtils;

/**
 * Programmatic benchmark orchestrator and execution engine for extensible test catalogs.
 * <p>
 * Allows orchestrating autonomous benchmark runs against candidate AI models under strict,
 * standardized environments. Spawns isolated child {@link Agi} sessions, automatically applies
 * toolkits and permissions, aggregates exact multi-turn token metrics, evaluates pass/fail status,
 * and persists results directly to designated JSON scorecards.
 * </p>
 *
 * @author anahata
 */
@Slf4j
// NOTE TO AI: Do NOT remove (Beta) or remove getToolkit().setEnabled(false). Keep disabled on startup by default.
@AgiToolkit("Programmatic benchmark orchestrator for extensible benchmark catalogs (Beta).")
public class Benchmarks extends AnahataToolkit {

    /**
     * The list of active registered test catalogs available in this benchmark session.
     * Marked transient to avoid serializing stale catalog definitions into Kryo session backups.
     */
    private transient List<TestCatalog> catalogs;

    /**
     * Default constructor for the Benchmarks toolkit.
     */
    public Benchmarks() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Disables the Benchmarks toolkit on startup by default and initializes fresh catalogs.
     * </p>
     */
    @Override
    public void initialize() {
        getToolkit().setEnabled(false);
        initCatalogs();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Rebinds transient catalog instances upon deserialization.
     * </p>
     */
    @Override
    public void rebind() {
        super.rebind();
        initCatalogs();
    }

    /**
     * Initializes or refreshes the default test catalogs, ensuring fresh code instances
     * are bound without relying on serialized Kryo state.
     */
    private synchronized void initCatalogs() {
        if (this.catalogs == null) {
            this.catalogs = new ArrayList<>();
        }
        this.catalogs.removeIf(c -> "ANAHATA-AGI-1".equalsIgnoreCase(c.getId()));
        this.catalogs.add(0, new Agi1TestCatalog());
    }

    /**
     * Retrieves the active list of registered test catalogs, ensuring initialization.
     *
     * @return The list of test catalogs.
     */
    public List<TestCatalog> getCatalogs() {
        if (catalogs == null) {
            initCatalogs();
        }
        return catalogs;
    }

    /**
     * Registers an additional test catalog with this benchmark engine.
     *
     * @param catalog The catalog to register.
     */
    public void registerCatalog(TestCatalog catalog) {
        if (catalog != null && !getCatalogs().contains(catalog)) {
            getCatalogs().add(catalog);
            log.info("Registered benchmark catalog: {} ({})", catalog.getName(), catalog.getId());
        }
    }

    /**
     * Finds a registered catalog by its identifier code.
     *
     * @param catalogId The catalog identifier.
     * @return Optional containing the catalog if found.
     */
    public Optional<TestCatalog> findCatalog(String catalogId) {
        if (catalogId == null || catalogId.isBlank()) {
            return Optional.empty();
        }
        return getCatalogs().stream()
                .filter(c -> c.getId().equalsIgnoreCase(catalogId.trim()) || c.getName().equalsIgnoreCase(catalogId.trim()))
                .findFirst();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Injects available benchmark catalogs, comprehensive test definitions,
     * the JSON results storage directory, and a summary of all recorded test runs into the RAG message.
     * </p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        try {
            StringBuilder sb = new StringBuilder("## Benchmark Suites & Catalogs\n\n");

            if (getCatalogs().isEmpty()) {
                sb.append("- No benchmark catalogs currently registered.\n");
            } else {
                for (TestCatalog catalog : getCatalogs()) {
                    sb.append(catalog.toString()).append("\n\n");
                }
            }

            ragMessage.addTextPart(sb.toString().trim());

        } catch (Exception e) {
            log.error("Error populating Benchmarks RAG message", e);
            ragMessage.addTextPart("## Benchmark Suites\n- ⚠️ Catalog fetch error: " + ExceptionUtils.getStackTrace(e) + "\n");
        }
    }

    /**
     * Sets the results directory for a specific registered catalog.
     *
     * @param catalogId The catalog identifier code (e.g. "ANAHATA-AGI-1").
     * @param resultsDirectory The directory path where results should be persisted.
     * @return Confirmation message.
     * @throws Exception If the catalog is unknown.
     */
    @AgiTool(value = "Sets the results storage directory for a specific benchmark catalog.", permission = ToolPermission.APPROVE_ALWAYS)
    public String setCatalogResultsDirectory(
            @AgiToolParam("The catalog identifier (e.g. 'ANAHATA-AGI-1').") String catalogId,
            @AgiToolParam(value = "The absolute filesystem path for results storage.", rendererId = "path") String resultsDirectory) throws Exception {
        TestCatalog catalog = findCatalog(catalogId)
                .orElseThrow(() -> new AgiToolException("Unknown benchmark catalog: " + catalogId));

        Path newPath = Paths.get(resultsDirectory);
        catalog.setResultsDirectory(newPath);
        refreshWebsiteArtifacts();
        return "Updated results directory for catalog '" + catalog.getName() + "' to: " + newPath.toAbsolutePath();
    }

    /**
     * Executes any registered benchmark test from any active catalog by its test code.
     *
     * @param testCode The test code (e.g., "JAVA-JNA-1", "JAVA-ARKANOID-1", "JAVA-SNAKEGAME-1").
     * @param participant The candidate participant descriptor.
     * @param openSession Whether to open the child AGI session tab in the UI.
     * @return The telemetry record of the benchmark run.
     * @throws Exception If benchmark orchestration fails or test code is unknown.
     */
    @AgiTool(value = "Runs a specific benchmark test from a registered catalog.", permission = ToolPermission.APPROVE_ALWAYS)
    public BenchmarkRunResult runTest(
            @AgiToolParam("The test code from the catalog (e.g., 'JAVA-JNA-1', 'JAVA-ARKANOID-1', 'JAVA-SNAKEGAME-1').") String testCode,
            @AgiToolParam("The candidate participant descriptor.") BenchmarkParticipant participant,
            @AgiToolParam(value = "Whether to open the child session tab in the UI.", required = false) boolean openSession) throws Exception {

        TestCatalog targetCatalog = null;
        TestDefinition targetTest = null;

        for (TestCatalog cat : getCatalogs()) {
            Optional<TestDefinition> found = cat.findByCode(testCode);
            if (found.isPresent()) {
                targetCatalog = cat;
                targetTest = found.get();
                break;
            }
        }

        if (targetTest == null) {
            throw new AgiToolException("Unknown benchmark test code across all catalogs: " + testCode);
        }

        return executeBenchmark(targetCatalog, targetTest, participant, openSession, true);
    }

    /**
     * Runs an ad-hoc benchmark on a custom test definition (prompt, title, isolated toolkits)
     * without persisting to official catalog scorecards or results.json.
     *
     * @param testDefinition The test definition specifying test code, title, raw prompt, and optional toolkit settings.
     * @param participant The candidate participant descriptor.
     * @param openSession Whether to open the child session tab in the UI during execution.
     * @return The complete telemetry record of the benchmark run.
     * @throws Exception If benchmark execution fails.
     */
    @AgiTool(value = "Runs an ad-hoc benchmark on a custom test definition (prompt, title, isolated toolkits) without persisting to official catalog scorecards.", permission = ToolPermission.APPROVE_ALWAYS)
    public BenchmarkRunResult runCustomPrompt(
            @AgiToolParam("The custom test definition DTO (testCode, title, rawPrompt, toolkits).") TestDefinition testDefinition,
            @AgiToolParam("The candidate participant descriptor.") BenchmarkParticipant participant,
            @AgiToolParam(value = "Whether to open the child session tab in the UI during execution.", required = false) boolean openSession) throws Exception {

        String testCode = (testDefinition.testCode() != null && !testDefinition.testCode().isBlank())
                ? testDefinition.testCode().trim()
                : "CUSTOM-" + (System.currentTimeMillis() % 100000);

        String title = (testDefinition.title() != null && !testDefinition.title().isBlank())
                ? testDefinition.title().trim()
                : "Custom Benchmark Challenge";

        TestDefinition effectiveTestDef = TestDefinition.builder()
                .testCode(testCode)
                .title(title)
                .rawPrompt(testDefinition.rawPrompt())
                .toolkits(testDefinition.toolkits())
                .build();

        return executeBenchmark(null, effectiveTestDef, participant, openSession, false);
    }

    /**
     * Convenience Java helper to run an ad-hoc benchmark on a custom prompt string and title without persisting.
     *
     * @param customPrompt The raw task prompt to benchmark the model with.
     * @param participant The candidate participant descriptor.
     * @param title Optional title for this custom challenge.
     * @param openSession Whether to open the child session tab in the UI during execution.
     * @return The complete telemetry record of the benchmark run.
     * @throws Exception If benchmark execution fails.
     */
    public BenchmarkRunResult runCustomPrompt(String customPrompt, BenchmarkParticipant participant, String title, boolean openSession) throws Exception {
        TestDefinition testDef = TestDefinition.builder()
                .title(title)
                .rawPrompt(customPrompt)
                .build();
        return runCustomPrompt(testDef, participant, openSession);
    }

    /**
     * Sequentially executes all registered benchmark tests in a specific catalog for a given candidate model.
     *
     * @param catalogId The catalog identifier code (e.g. "ANAHATA-AGI-1").
     * @param participant The candidate participant descriptor.
     * @param openSession Whether to open child session tabs in the UI.
     * @return A list of telemetry records for all executed tests in that catalog.
     * @throws Exception If any benchmark execution fails or catalog is not found.
     */
    @AgiTool(value = "Sequentially executes all registered benchmark tests in a specific catalog for a candidate model.", permission = ToolPermission.APPROVE_ALWAYS)
    public List<BenchmarkRunResult> runCatalog(
            @AgiToolParam("The catalog identifier (e.g. 'ANAHATA-AGI-1').") String catalogId,
            @AgiToolParam("The candidate participant descriptor.") BenchmarkParticipant participant,
            @AgiToolParam(value = "Whether to open child session tabs in the UI.", required = false) boolean openSession) throws Exception {
        TestCatalog catalog = findCatalog(catalogId)
                .orElseThrow(() -> new AgiToolException("Unknown benchmark catalog: " + catalogId));

        List<BenchmarkRunResult> results = new ArrayList<>();
        for (TestDefinition testDef : catalog.getTests()) {
            log("Starting benchmark test: " + testDef.testCode() + " (" + testDef.title() + ") in catalog " + catalog.getName());
            BenchmarkRunResult result = executeBenchmark(catalog, testDef, participant, openSession, true);
            results.add(result);
        }

        return results;
    }

    /**
     * Submits or updates a judge's subjective score for a specific benchmark test run, keyed by session ID.
     *
     * @param sessionId The unique session ID of the run to score.
     * @param judgeScore The judge score DTO (name, score, comments).
     * @return A confirmation message indicating whether the score was updated.
     * @throws Exception If updating the results store fails.
     */
    @AgiTool(value = "Submits or updates a judge's score for a candidate run in the results database.", permission = ToolPermission.APPROVE_ALWAYS)
    public String submitJudgeScore(
            @AgiToolParam("The unique session ID of the benchmark run to score.") String sessionId,
            @AgiToolParam("The judge score DTO (name, score, comments).") JudgeScore judgeScore) throws Exception {

        for (TestCatalog cat : getCatalogs()) {
            for (TestDefinition test : cat.getTests()) {
                Path resultsFile = cat.getResultsFileForTest(test.testCode());
                if (Files.exists(resultsFile) && BenchmarkResultsStore.submitJudgeScore(resultsFile, sessionId, judgeScore)) {
                    refreshWebsiteArtifacts();
                    return "Successfully recorded judge score of " + judgeScore.score() + " by " + judgeScore.name() + " for session " + sessionId;
                }
            }
        }
        return "No matching benchmark run found for session " + sessionId + ". Execute the test first before scoring.";
    }

    /**
     * Sets or updates the qualitative observations notes for a specific benchmark test run, matching by session ID.
     *
     * @param sessionId The unique session ID of the benchmark run.
     * @param observations The qualitative observations, tool output notes, or defect descriptions to record.
     * @return Confirmation message indicating whether the observations were updated.
     * @throws Exception If updating the results store fails.
     */
    @AgiTool(value = "Sets or updates the observations notes for a specific benchmark test run, matching by session ID.", permission = ToolPermission.APPROVE_ALWAYS)
    public String setObservations(
            @AgiToolParam("The unique session ID of the benchmark run.") String sessionId,
            @AgiToolParam("The qualitative observations, tool output notes, or defect descriptions to record.") String observations) throws Exception {

        for (TestCatalog cat : getCatalogs()) {
            for (TestDefinition test : cat.getTests()) {
                Path resultsFile = cat.getResultsFileForTest(test.testCode());
                if (Files.exists(resultsFile) && BenchmarkResultsStore.setObservations(resultsFile, sessionId, observations)) {
                    refreshWebsiteArtifacts();
                    return "Successfully updated observations for session " + sessionId + " on test " + test.testCode();
                }
            }
        }
        return "No matching benchmark run found for session " + sessionId + ". Execute the test first before recording observations.";
    }

    /**
     * Deletes a recorded benchmark run from the results database across all catalogs by its session ID.
     *
     * @param sessionId The unique session ID of the benchmark run to delete.
     * @return Confirmation message indicating whether the run was deleted.
     * @throws Exception If deleting fails or if no matching run is found.
     */
    @AgiTool(value = "Deletes a recorded benchmark run from the results database by its session ID.", permission = ToolPermission.APPROVE_ALWAYS)
    public String deleteResult(
            @AgiToolParam("The unique session ID of the benchmark run to delete.") String sessionId) throws Exception {

        for (TestCatalog cat : getCatalogs()) {
            for (TestDefinition test : cat.getTests()) {
                Path resultsFile = cat.getResultsFileForTest(test.testCode());
                if (Files.exists(resultsFile) && BenchmarkResultsStore.deleteResult(resultsFile, sessionId)) {
                    refreshWebsiteArtifacts();
                    return "Successfully deleted benchmark run for session " + sessionId + " from test " + test.testCode();
                }
            }
        }
        throw new AgiToolException("No matching benchmark run found for session " + sessionId + " across any catalogs.");
    }

    /**
     * Lists all recorded benchmark runs and scores for a specific test code across all catalogs.
     *
     * @param testCode The test code (e.g. "JAVA-JNA-1").
     * @return The list of recorded runs.
     * @throws Exception If reading the results store fails.
     */
    @AgiTool(value = "Lists all recorded benchmark runs and scores for a specific test code.", permission = ToolPermission.APPROVE_ALWAYS)
    public List<BenchmarkRunResult> listResults(
            @AgiToolParam("The test code (e.g., 'JAVA-JNA-1', 'JAVA-ARKANOID-1').") String testCode) throws Exception {
        for (TestCatalog cat : getCatalogs()) {
            if (cat.findByCode(testCode).isPresent()) {
                return BenchmarkResultsStore.loadResults(cat, testCode);
            }
        }
        return List.of();
    }

    /**
     * Finds benchmark run results across all catalogs matching ALL provided predicates (AND semantics).
     * <p>
     * Every non-null predicate must match for a run to be returned.
     * </p>
     *
     * @param testCode The optional test code filter (case-insensitive).
     * @param providerUuid The optional provider UUID filter (case-insensitive).
     * @param modelId The optional model ID filter (case-insensitive).
     * @param passed The optional passed/failed status filter.
     * @param sessionId The optional session ID filter (case-insensitive).
     * @return The list of runs matching every provided predicate.
     * @throws Exception If reading the results store fails.
     */
    @AgiTool(value = "Finds benchmark run results matching ALL provided predicates (AND semantics).", permission = ToolPermission.APPROVE_ALWAYS)
    public List<BenchmarkRunResult> findResults(
            @AgiToolParam(value = "Optional test code filter (e.g. 'JAVA-JNA-1').", required = false) String testCode,
            @AgiToolParam(value = "Optional provider UUID filter.", required = false) String providerUuid,
            @AgiToolParam(value = "Optional model ID filter.", required = false) String modelId,
            @AgiToolParam(value = "Optional passed/failed status filter.", required = false) Boolean passed,
            @AgiToolParam(value = "Optional session ID filter.", required = false) String sessionId) throws Exception {
        List<BenchmarkRunResult> matches = new ArrayList<>();
        for (TestCatalog cat : getCatalogs()) {
            for (TestDefinition test : cat.getTests()) {
                if (testCode != null && !testCode.isBlank() && !test.testCode().equalsIgnoreCase(testCode.trim())) {
                    continue;
                }
                Path resultsFile = cat.getResultsFileForTest(test.testCode());
                matches.addAll(BenchmarkResultsStore.findResults(resultsFile, providerUuid, modelId, passed, sessionId));
            }
        }
        return matches;
    }

    /**
     * Updates an entire benchmark run result record on disk, matching by session ID (the unique primary key).
     *
     * @param result The fully populated replacement {@link BenchmarkRunResult}.
     * @return A confirmation message indicating whether the record was replaced.
     * @throws Exception If updating the results store fails.
     */
    @AgiTool(value = "Updates an entire benchmark run result JSON record on disk, matching by session ID.", permission = ToolPermission.APPROVE_ALWAYS)
    public String updateResults(
            @AgiToolParam("The fully populated BenchmarkRunResult to persist, replacing the matching existing record by session ID.") BenchmarkRunResult result) throws Exception {
        for (TestCatalog cat : getCatalogs()) {
            if (cat.findByCode(result.testCode()).isPresent()) {
                boolean updated = BenchmarkResultsStore.updateResult(cat.getResultsFileForTest(result.testCode()), result);
                if (updated) {
                    refreshWebsiteArtifacts();
                    return "Successfully updated benchmark result for session " + result.sessionId() + " on " + result.testCode();
                }
            }
        }
        return "No matching benchmark run found for session " + result.sessionId() + " on " + result.testCode() + ". Record the run first before updating.";
    }

    /**
     * Internal execution harness that provisions the child AGI, executes the test autonomously,
     * harvests fine-grained telemetry, and conditionally persists the result to the matching catalog.
     *
     * @param catalog The catalog owning the test, or {@code null} for ad-hoc custom runs.
     * @param testDef The test definition.
     * @param participant The candidate participant.
     * @param openSession Whether to open the session UI.
     * @param persistResults Whether to record results into the catalog results file and results.json.
     * @return The complete benchmark run result.
     * @throws Exception If an unrecoverable execution error occurs.
     */
    private BenchmarkRunResult executeBenchmark(TestCatalog catalog, TestDefinition testDef, BenchmarkParticipant participant, boolean openSession, boolean persistResults) throws Exception {
        final ToolContext ctx = getToolContext();
        AbstractAsiContainer container = getAsiContainer();

        AgiConfig config = container.createNewAgiConfig();
        config.setSelectedProviderUuid(participant.providerUuid());
        config.setSelectedModelId(participant.modelId());
        config.setAutoReplyTools(true);
        config.setParentUuid(getAgi().getConfig().getSessionId());

        // Isolated toolkits defined strictly by the test specification (null/empty inherits container defaults)
        Map<String, ToolPermission> permissionOverrides = new HashMap<>();
        if (testDef.toolkits() != null && !testDef.toolkits().isEmpty()) {
            config.getToolClasses().clear();
            for (ToolkitSettings ts : testDef.toolkits()) {
                Class<?> baseClass = Class.forName(ts.toolkit());
                Class<?> concreteClass = getAgi().getToolkit(baseClass)
                        .map(Object::getClass)
                        .orElse(baseClass);
                config.getToolClasses().add(concreteClass);
                permissionOverrides.putAll(ts.getResolvedPermissions(concreteClass));
            }
        }

        ctx.log("Spawning candidate AGI session for test: " + testDef.testCode() + " with model: " + participant.modelId());
        Agi candidateAgi = container.createNewAgi(config);
        candidateAgi.setNickname("Bench: " + testDef.testCode() + " - " + participant.modelId());
        candidateAgi.getRequestConfig().setThinkingLevel(participant.thinkingLevel());
        ctx.log("Candidate AGI session spawned with ID: " + candidateAgi.getConfig().getSessionId() + " (" + candidateAgi.getShortId() + ")");

        // Apply strict tool permission overrides if toolkits are explicitly configured
        permissionOverrides.forEach((toolName, permission) -> {
            candidateAgi.getToolManager().findToolByName(toolName)
                    .ifPresent(tool -> tool.setPermission(permission));
        });

        if (!openSession) {
            container.close(candidateAgi);
        }

        // Headless execution fallback
        if (GraphicsEnvironment.isHeadless()) {
            return executeAutonomousDirectRun(catalog, candidateAgi, testDef, participant, ctx, persistResults);
        }

        ScreenRecorder recorder = new ScreenRecorder();
        CompletableFuture<BenchmarkRunResult> runResultFuture = new CompletableFuture<>();

        Thread[] candidateThreadHolder = new Thread[1];
        AtomicBoolean executionFinished = new AtomicBoolean(false);

        ScreenRecordingOverlay[] overlayHolder = new ScreenRecordingOverlay[1];
        overlayHolder[0] = new ScreenRecordingOverlay(
                testDef.testCode(),
                participant.modelId(),
                // onStartAction: Start FFmpeg recording on chosen screen and launch benchmark turn
                () -> {
                    try {
                        int deviceIdx = overlayHolder[0].getSelectedDeviceIndex();
                        ctx.log("Starting screen recording on Screen " + deviceIdx + " for " + testDef.testCode());
                        recorder.startRecording(testDef.testCode(), participant.modelId(), deviceIdx);

                        candidateThreadHolder[0] = Thread.currentThread();
                        executeCandidateTurn(catalog, candidateAgi, testDef, ctx);
                        executionFinished.set(true);
                        ctx.log("Candidate AGI execution completed. Candidate window is live. Waiting for user demonstration & stop...");
                    } catch (Exception e) {
                        log.error("Error during candidate AGI execution", e);
                        ctx.error(e);
                    }
                },
                // onSaveLocalAction: Finalize MP4 & save result locally without YouTube upload
                () -> {
                    try {
                        ctx.log("Finalizing recording (Save Local)...");
                        RecordedSession session = recorder.stopRecording(true, null);
                        double duration = session != null ? session.durationSeconds() : 0.0;
                        String localVideoPath = session != null && session.videoPath() != null ? session.videoPath().toString() : null;
                        String thumbPath = session != null && session.thumbnailPath() != null ? session.thumbnailPath().toString() : null;
                        if (localVideoPath != null) {
                            ctx.log("Recording saved to disk at: " + localVideoPath);
                        }
                        BenchmarkRunResult runResult = compileRunResult(candidateAgi, testDef, participant, duration, thumbPath, null);
                        if (persistResults) {
                            BenchmarkResultsStore.recordResult(catalog, runResult);
                        }
                        runResultFuture.complete(runResult);
                    } catch (Exception e) {
                        log.error("Failed to save local benchmark result", e);
                        runResultFuture.completeExceptionally(e);
                    }
                },
                // onUploadAction: Finalize MP4, upload to YouTube, set thumbnail, add to playlist, save results.json
                () -> {
                    try {
                        ctx.log("Finalizing recording and uploading to YouTube...");
                        RecordedSession session = recorder.stopRecording(true, null);
                        double duration = session != null ? session.durationSeconds() : 0.0;
                        String localVideoPath = session != null && session.videoPath() != null ? session.videoPath().toString() : null;
                        String thumbPath = session != null && session.thumbnailPath() != null ? session.thumbnailPath().toString() : null;
                        if (localVideoPath != null) {
                            ctx.log("Recording saved to disk at: " + localVideoPath);
                        }
                        // Compile metrics first so token and turn stats can be enriched into the YouTube description
                        BenchmarkRunResult rawMetrics = compileRunResult(candidateAgi, testDef, participant, duration, thumbPath, null);
                        String videoUrl = null;
                        if (session != null && session.videoPath() != null) {
                            videoUrl = uploadBenchmarkVideoToYouTube(catalog, testDef, participant, session, rawMetrics, ctx);
                        }
                        BenchmarkRunResult runResult = BenchmarkRunResult.builder()
                                .participant(rawMetrics.participant())
                                .testCode(rawMetrics.testCode())
                                .asiContainer(rawMetrics.asiContainer())
                                .timestamp(rawMetrics.timestamp())
                                .durationSeconds(rawMetrics.durationSeconds())
                                .turns(rawMetrics.turns())
                                .promptTokens(rawMetrics.promptTokens())
                                .candidatesTokens(rawMetrics.candidatesTokens())
                                .thoughtsTokens(rawMetrics.thoughtsTokens())
                                .totalTokens(rawMetrics.totalTokens())
                                .passed(rawMetrics.passed())
                                .judgeScores(rawMetrics.judgeScores())
                                .videoUrl(videoUrl)
                                .screenshotPath(rawMetrics.screenshotPath())
                                .sessionId(rawMetrics.sessionId())
                                .observations(rawMetrics.observations())
                                .build();
                        if (persistResults) {
                            BenchmarkResultsStore.recordResult(catalog, runResult);
                        }
                        runResultFuture.complete(runResult);
                    } catch (Exception e) {
                        log.error("Failed to upload benchmark result to YouTube", e);
                        runResultFuture.completeExceptionally(e);
                    }
                },
                // onCancelAction: Discard recording and cancel
                () -> {
                    ctx.log("Benchmark run cancelled by tester.");
                    recorder.cancelRecording();
                    if (candidateThreadHolder[0] != null && !executionFinished.get()) {
                        candidateThreadHolder[0].interrupt();
                    }
                    runResultFuture.completeExceptionally(new AgiToolException("Benchmark recording and execution cancelled by tester."));
                }
        ).withCustomLabels("▶ Start Recording & Run Benchmark", "💾 Save", "🚀 Save & Upload", "❌ Cancel");

        overlayHolder[0].showPreLaunch();

        // Wait for tester to complete recording via overlay buttons (no default timeout enforcement)
        return runResultFuture.get();
    }

    /**
     * Executes the candidate AGI turn by formatting the standardized prompt from the catalog or using the raw prompt.
     *
     * @param catalog The catalog owning the test templates, or {@code null} for custom runs.
     * @param candidateAgi The child session.
     * @param testDef The test definition.
     * @param ctx The captured tool execution context.
     */
    private void executeCandidateTurn(TestCatalog catalog, Agi candidateAgi, TestDefinition testDef, ToolContext ctx) {
        String prompt = (catalog != null) ? catalog.formatPrompt(testDef, candidateAgi) : testDef.rawPrompt();
        ctx.log("Submitting benchmark prompt to candidate AGI session: " + candidateAgi.getConfig().getSessionId() + " (" + candidateAgi.getShortId() + ")");
        AgiUserMessage userMsg = new AgiUserMessage(candidateAgi, getAgi().getConfig().getSessionId());
        userMsg.addTextPart(prompt);
        candidateAgi.sendMessage(userMsg);
    }

    /**
     * Headless fallback execution path.
     *
     * @param catalog The catalog context, or {@code null} for custom runs.
     * @param candidateAgi The child session.
     * @param testDef The test definition.
     * @param participant The participant.
     * @param ctx The captured tool execution context.
     * @param persistResults Whether to record results into the catalog scorecard.
     * @return The benchmark run result.
     * @throws Exception If execution fails.
     */
    private BenchmarkRunResult executeAutonomousDirectRun(TestCatalog catalog, Agi candidateAgi, TestDefinition testDef, BenchmarkParticipant participant, ToolContext ctx, boolean persistResults) throws Exception {
        long startMillis = System.currentTimeMillis();
        executeCandidateTurn(catalog, candidateAgi, testDef, ctx);
        long durationMillis = System.currentTimeMillis() - startMillis;
        double durationSeconds = Math.round((durationMillis / 1000.0) * 100.0) / 100.0;

        BenchmarkRunResult runResult = compileRunResult(candidateAgi, testDef, participant, durationSeconds, null, null);
        if (persistResults) {
            BenchmarkResultsStore.recordResult(catalog, runResult);
        }
        return runResult;
    }

    /**
     * Uploads the recorded benchmark demonstration video to YouTube with metadata, telemetry description, and thumbnail.
     *
     * @param catalog The catalog owning the test, or {@code null} for custom runs.
     * @param testDef The test definition.
     * @param participant The participant descriptor.
     * @param session The recorded video session.
     * @param metrics The compiled telemetry metrics for the run.
     * @param ctx The captured tool context for pass-through logging.
     * @return The uploaded YouTube video URL, or {@code null} if authentication is missing.
     */
    private String uploadBenchmarkVideoToYouTube(TestCatalog catalog, TestDefinition testDef, BenchmarkParticipant participant, RecordedSession session, BenchmarkRunResult metrics, ToolContext ctx) {
        try {
            YouTubeCredentials creds = YouTubeCredentials.load();
            if (!creds.isAuthenticated()) {
                ctx.log("YouTube is not authenticated. Video saved locally at: " + session.videoPath());
                return null;
            }

            String catalogName = (catalog != null && catalog.getName() != null) ? catalog.getName() : "Custom Challenge";
            String catalogId = (catalog != null && catalog.getId() != null) ? catalog.getId() : "CUSTOM";
            String catalogUrlCode = catalogId.toLowerCase().replace('_', '-');

            String title = "⚡ " + catalogName + ": " + participant.modelId() + " on " + testDef.testCode() + " (" + testDef.title() + ")";

            String statusStr = (metrics != null && metrics.passed()) ? "✅ PASSED (Zero Defects)" : "❌ FAILED";
            int promptTokens = metrics != null ? metrics.promptTokens() : 0;
            int candidatesTokens = metrics != null ? metrics.candidatesTokens() : 0;
            int thoughtsTokens = metrics != null ? metrics.thoughtsTokens() : 0;
            int totalTokens = metrics != null ? metrics.totalTokens() : 0;
            int turns = metrics != null ? metrics.turns() : 0;
            double duration = metrics != null ? metrics.durationSeconds() : session.durationSeconds();
            String container = metrics != null && metrics.asiContainer() != null ? metrics.asiContainer() : getAsiContainer().getClass().getSimpleName();

            StringBuilder desc = new StringBuilder();
            desc.append("⚡ ").append(catalogName).append(" Benchmark Run: ").append(testDef.testCode()).append("\n");
            desc.append("==================================================\n");
            desc.append("🎯 CHALLENGE: ").append(testDef.title()).append("\n");
            desc.append("🤖 MODEL: ").append(participant.modelId()).append("\n");
            desc.append("🏢 PROVIDER: ").append(participant.providerUuid()).append("\n");
            desc.append("🧠 THINKING LEVEL: ").append(participant.thinkingLevel()).append("\n");
            desc.append("💻 CONTAINER: ").append(container).append("\n");
            desc.append("--------------------------------------------------\n");
            desc.append("📈 TELEMETRY METRICS:\n");
            desc.append("• Status: ").append(statusStr).append("\n");
            desc.append("• Duration: ").append(duration).append("s\n");
            desc.append("• Interaction Turns: ").append(turns).append("\n");
            desc.append("• Total Tokens: ").append(String.format("%,d", totalTokens)).append("\n");
            desc.append("  - Prompt (Input): ").append(String.format("%,d", promptTokens)).append("\n");
            desc.append("  - Candidate (Output): ").append(String.format("%,d", candidatesTokens)).append("\n");
            desc.append("  - Thoughts (Reasoning): ").append(String.format("%,d", thoughtsTokens)).append("\n");
            desc.append("--------------------------------------------------\n");
            if (catalog != null) {
                desc.append("📊 Interactive Telemetry & Leaderboard:\n");
                desc.append("https://asi.anahata.uno/benchmarks/").append(catalogUrlCode).append("/index.html?test=").append(testDef.testCode()).append("\n\n");
                desc.append("🏆 Master Suite Leaderboard:\n");
                desc.append("https://asi.anahata.uno/benchmarks/").append(catalogUrlCode).append("/index.html\n\n");
            }

            if (metrics != null && metrics.observations() != null && !metrics.observations().isBlank()) {
                desc.append("⚠️ Observations / Error Details:\n").append(metrics.observations()).append("\n\n");
            }

            desc.append("Prompt:\n\"").append(testDef.rawPrompt()).append("\"\n\n");
            desc.append("#AnahataASI #Java #AI #Benchmarks #LLM #OpenSource #ForcaBarca");

            List<String> tags = List.of("AnahataASI", "Java", "AI", "Benchmarks", "LLM", testDef.testCode(), participant.providerUuid());

            YouTube youtube = getAgi().getToolkit(YouTube.class).orElse(new YouTube());

            String playlistId = creds.playlistId();
            if (catalog != null) {
                try {
                    String playlistTitle = catalogName + ": " + testDef.testCode();
                    playlistId = youtube.resolveOrCreatePlaylist(playlistTitle,
                            "Automated " + catalogName + " benchmark runs for " + testDef.testCode() + " (" + testDef.title() + ").");
                    ctx.log("Resolved per-test playlist '" + playlistTitle + "' -> " + playlistId);
                } catch (Exception e) {
                    log.error("Could not resolve per-test playlist; falling back to default playlist", e);
                    ctx.error("Could not resolve per-test playlist; falling back to default playlist: " + e.getMessage());
                }
            }

            YouTubeVideoUploadRequest request = YouTubeVideoUploadRequest.builder()
                    .videoFilePath(session.videoPath().toString())
                    .title(title)
                    .description(desc.toString())
                    .tags(tags)
                    .playlistId(playlistId)
                    .privacyStatus("public")
                    .build();

            String videoUrl = youtube.uploadVideo(request);
            ctx.log("YouTube video published: " + videoUrl);

            if (session.thumbnailPath() != null) {
                try {
                    String videoId = videoUrl.substring(videoUrl.lastIndexOf('/') + 1);
                    youtube.setThumbnail(videoId, session.thumbnailPath().toString());
                    ctx.log("Custom thumbnail set for video: " + videoId);
                } catch (Exception e) {
                    log.warn("Could not set custom thumbnail on YouTube", e);
                }
            }

            return videoUrl;
        } catch (Exception e) {
            log.error("Failed to publish video to YouTube", e);
            return null;
        }
    }

    /**
     * Compiles the complete {@link BenchmarkRunResult} across the candidate AGI's conversation history.
     *
     * @param candidateAgi The child session.
     * @param testDef The test definition.
     * @param participant The candidate participant descriptor.
     * @param durationSeconds The wall-clock execution duration in seconds.
     * @param screenshotPath The captured screenshot thumbnail path.
     * @param videoUrl The published video URL.
     * @return The populated {@link BenchmarkRunResult}.
     */
    private BenchmarkRunResult compileRunResult(Agi candidateAgi, TestDefinition testDef, BenchmarkParticipant participant, double durationSeconds, String screenshotPath, String videoUrl) {
        int promptTokens = 0;
        int candidatesTokens = 0;
        int thoughtsTokens = 0;
        int totalTokens = 0;
        int turns = 0;
        boolean passed = true;
        StringBuilder observations = new StringBuilder();

        for (AbstractMessage msg : candidateAgi.getContextManager().getHistory()) {
            if (msg instanceof AbstractModelMessage<?> modelMsg) {
                turns++;
                Response<?> response = modelMsg.getResponse();
                if (response != null) {
                    ResponseUsageMetadata usage = response.getUsageMetadata();
                    if (usage != null) {
                        promptTokens += usage.getPromptTokenCount();
                        candidatesTokens += usage.getCandidatesTokenCount();
                        thoughtsTokens += usage.getThoughtsTokenCount();
                        totalTokens += usage.getTotalTokenCount();
                    } else {
                        totalTokens += response.getTotalTokenCount();
                    }
                }

                // Check for any failed tool executions
                for (AbstractToolCall<?, ?> call : modelMsg.getToolCalls()) {
                    if (call.getResponse() != null) {
                        if (call.getResponse().getErrors() != null && !call.getResponse().getErrors().isBlank()) {
                            passed = false;
                            observations.append("Tool error in ").append(call.getToolName()).append(": ").append(call.getResponse().getErrors()).append("\n");
                        }
                    }
                }
            }
        }

        if (turns == 0) {
            passed = false;
            observations.append("No response turns received from candidate model.\n");
        }

        return BenchmarkRunResult.builder()
                .participant(participant)
                .testCode(testDef.testCode())
                .asiContainer(getAsiContainer().getClass().getSimpleName())
                .timestamp(Instant.now())
                .durationSeconds(durationSeconds)
                .turns(turns)
                .promptTokens(promptTokens)
                .candidatesTokens(candidatesTokens)
                .thoughtsTokens(thoughtsTokens)
                .totalTokens(totalTokens > 0 ? totalTokens : (promptTokens + candidatesTokens + thoughtsTokens))
                .passed(passed)
                .judgeScores(new ArrayList<>())
                .videoUrl(videoUrl)
                .screenshotPath(screenshotPath)
                .sessionId(candidateAgi.getConfig().getSessionId())
                .observations(observations.toString().trim())
                .build();
    }

    /**
     * Regenerates the public website benchmark artifacts ({@code catalog.json} and {@code results.json})
     * for every registered catalog, keeping the static leaderboard in sync with the engine.
     */
    private void refreshWebsiteArtifacts() {
        for (TestCatalog catalog : getCatalogs()) {
            try {
                BenchmarkResultsStore.refreshWebsiteManifests(catalog);
            } catch (Exception e) {
                log.error("Failed to refresh website benchmark artifacts for catalog " + catalog.getId(), e);
            }
        }
    }
}
