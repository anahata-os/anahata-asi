/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.benchmarks;

import java.awt.GraphicsEnvironment;
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
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.agi.tool.spi.java.JavaObjectToolkit;
import uno.anahata.asi.toolkit.java.Java;
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
     */
    private final List<TestCatalog> catalogs = new ArrayList<>(List.of(new Agi1TestCatalog()));

    /**
     * Default constructor for the Benchmarks toolkit.
     */
    public Benchmarks() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Disables the Benchmarks toolkit on startup by default.
     * </p>
     */
    @Override
    public void initialize() {
        getToolkit().setEnabled(false);
    }

    /**
     * Registers an additional test catalog with this benchmark engine.
     *
     * @param catalog The catalog to register.
     */
    public void registerCatalog(TestCatalog catalog) {
        if (catalog != null && !catalogs.contains(catalog)) {
            catalogs.add(catalog);
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
        return catalogs.stream()
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

            if (catalogs.isEmpty()) {
                sb.append("- No benchmark catalogs currently registered.\n");
            } else {
                for (TestCatalog catalog : catalogs) {
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

        for (TestCatalog cat : catalogs) {
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

        return executeBenchmark(targetCatalog, targetTest, participant, openSession);
    }

    /**
     * Runs an ad-hoc benchmark on a custom prompt with container default toolkits and permissions.
     *
     * @param customPrompt The raw task prompt to benchmark the model with.
     * @param participant The candidate participant descriptor.
     * @param title Optional title for this custom challenge.
     * @param openSession Whether to open the child session tab in the UI during execution.
     * @return The complete telemetry record of the benchmark run.
     * @throws Exception If benchmark execution fails.
     */
    @AgiTool(value = "Runs an ad-hoc benchmark on a custom prompt with container default toolkits and permissions.", permission = ToolPermission.APPROVE_ALWAYS)
    public BenchmarkRunResult runCustomPrompt(
            @AgiToolParam("The raw task prompt to benchmark the model with.") String customPrompt,
            @AgiToolParam("The candidate participant descriptor.") BenchmarkParticipant participant,
            @AgiToolParam(value = "Optional title for this custom challenge.", required = false) String title,
            @AgiToolParam(value = "Whether to open the child session tab in the UI during execution.", required = false) boolean openSession) throws Exception {

        String effectiveTitle = (title != null && !title.isBlank()) ? title.trim() : "Custom Benchmark Challenge";
        String testCode = "CUSTOM-" + (System.currentTimeMillis() % 100000);

        TestDefinition customTestDef = TestDefinition.builder()
                .testCode(testCode)
                .title(effectiveTitle)
                .rawPrompt(customPrompt)
                .toolkits(null) // Null inherits container defaults cleanly
                .build();

        TestCatalog primaryCatalog = !catalogs.isEmpty() ? catalogs.get(0) : new Agi1TestCatalog();
        return executeBenchmark(primaryCatalog, customTestDef, participant, openSession);
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
            BenchmarkRunResult result = executeBenchmark(catalog, testDef, participant, openSession);
            results.add(result);
        }

        return results;
    }

    /**
     * Submits or updates a judge's subjective score for a specific benchmark test run.
     *
     * @param testCode The test code (e.g. "JAVA-JNA-1").
     * @param participant The candidate participant descriptor.
     * @param judgeName The name of the judge (e.g., "Pablo", "Vijay").
     * @param score The score awarded by the judge (e.g., 9.5).
     * @return A confirmation message indicating whether the score was updated.
     * @throws Exception If updating the results store fails.
     */
    @AgiTool(value = "Submits or updates a judge's score for a candidate run in the results database.", permission = ToolPermission.APPROVE_ALWAYS)
    public String submitJudgeScore(
            @AgiToolParam("The test code (e.g., 'JAVA-JNA-1').") String testCode,
            @AgiToolParam("The candidate participant descriptor.") BenchmarkParticipant participant,
            @AgiToolParam("The judge name (e.g., 'Pablo', 'Vijay').") String judgeName,
            @AgiToolParam("The score (0.0 to 10.0 or 0 to 100).") double score) throws Exception {

        for (TestCatalog cat : catalogs) {
            if (cat.findByCode(testCode).isPresent()) {
                boolean updated = BenchmarkResultsStore.submitJudgeScore(cat, testCode, participant, judgeName, score);
                if (updated) {
                    return "Successfully recorded judge score of " + score + " by " + judgeName + " for " + participant + " on " + testCode;
                }
            }
        }
        return "No matching benchmark run found for " + participant + " on " + testCode + ". Execute the test first before scoring.";
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
        for (TestCatalog cat : catalogs) {
            if (cat.findByCode(testCode).isPresent()) {
                return BenchmarkResultsStore.loadResults(cat, testCode);
            }
        }
        return List.of();
    }

    /**
     * Internal execution harness that provisions the child AGI, executes the test autonomously,
     * harvests fine-grained telemetry, and persists the result to the matching catalog.
     *
     * @param catalog The catalog owning the test.
     * @param testDef The test definition.
     * @param participant The candidate participant.
     * @param openSession Whether to open the session UI.
     * @return The complete benchmark run result.
     * @throws Exception If an unrecoverable execution error occurs.
     */
    private BenchmarkRunResult executeBenchmark(TestCatalog catalog, TestDefinition testDef, BenchmarkParticipant participant, boolean openSession) throws Exception {
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

        log("Spawning candidate AGI session for test: " + testDef.testCode() + " with model: " + participant.modelId());
        Agi candidateAgi = container.createNewAgi(config);
        candidateAgi.setNickname("Bench: " + testDef.testCode() + " - " + participant.modelId());
        candidateAgi.getRequestConfig().setThinkingLevel(participant.thinkingLevel());

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
            return executeAutonomousDirectRun(catalog, candidateAgi, testDef, participant);
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
                        log("Starting screen recording on Screen " + deviceIdx + " for " + testDef.testCode());
                        recorder.startRecording(testDef.testCode(), participant.modelId(), deviceIdx);

                        candidateThreadHolder[0] = Thread.currentThread();
                        executeCandidateTurn(catalog, candidateAgi, testDef);
                        executionFinished.set(true);
                        log("Candidate AGI execution completed. Candidate window is live. Waiting for user demonstration & stop...");
                    } catch (Exception e) {
                        log.error("Error during candidate AGI execution", e);
                        error(e);
                    }
                },
                // onSaveLocalAction: Finalize MP4 & save result locally without YouTube upload
                () -> {
                    try {
                        log("Finalizing recording (Save Local)...");
                        RecordedSession session = recorder.stopRecording(true, null);
                        double duration = session != null ? session.durationSeconds() : 0.0;
                        String thumbPath = session != null && session.thumbnailPath() != null ? session.thumbnailPath().toString() : null;
                        BenchmarkRunResult runResult = compileRunResult(candidateAgi, testDef, participant, duration, thumbPath, null);
                        BenchmarkResultsStore.recordResult(catalog, runResult);
                        runResultFuture.complete(runResult);
                    } catch (Exception e) {
                        log.error("Failed to save local benchmark result", e);
                        runResultFuture.completeExceptionally(e);
                    }
                },
                // onUploadAction: Finalize MP4, upload to YouTube, set thumbnail, add to playlist, save results.json
                () -> {
                    try {
                        log("Finalizing recording and uploading to YouTube...");
                        RecordedSession session = recorder.stopRecording(true, null);
                        String videoUrl = null;
                        if (session != null && session.videoPath() != null) {
                            videoUrl = uploadBenchmarkVideoToYouTube(testDef, participant, session);
                        }
                        double duration = session != null ? session.durationSeconds() : 0.0;
                        String thumbPath = session != null && session.thumbnailPath() != null ? session.thumbnailPath().toString() : null;
                        BenchmarkRunResult runResult = compileRunResult(candidateAgi, testDef, participant, duration, thumbPath, videoUrl);
                        BenchmarkResultsStore.recordResult(catalog, runResult);
                        runResultFuture.complete(runResult);
                    } catch (Exception e) {
                        log.error("Failed to upload benchmark result to YouTube", e);
                        runResultFuture.completeExceptionally(e);
                    }
                },
                // onCancelAction: Discard recording and cancel
                () -> {
                    log("Benchmark run cancelled by tester.");
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
     * Executes the candidate AGI turn by formatting the standardized prompt from the catalog.
     *
     * @param catalog The catalog owning the test templates.
     * @param candidateAgi The child session.
     * @param testDef The test definition.
     */
    private void executeCandidateTurn(TestCatalog catalog, Agi candidateAgi, TestDefinition testDef) {
        String prompt = catalog.formatPrompt(testDef);
        log("Submitting official benchmark prompt to candidate AGI: " + candidateAgi.getShortId());
        AgiUserMessage userMsg = new AgiUserMessage(candidateAgi, getAgi().getConfig().getSessionId());
        userMsg.addTextPart(prompt);
        candidateAgi.sendMessage(userMsg);
    }

    /**
     * Headless fallback execution path.
     *
     * @param catalog The catalog context.
     * @param candidateAgi The child session.
     * @param testDef The test definition.
     * @param participant The participant.
     * @return The benchmark run result.
     * @throws Exception If execution fails.
     */
    private BenchmarkRunResult executeAutonomousDirectRun(TestCatalog catalog, Agi candidateAgi, TestDefinition testDef, BenchmarkParticipant participant) throws Exception {
        long startMillis = System.currentTimeMillis();
        executeCandidateTurn(catalog, candidateAgi, testDef);
        long durationMillis = System.currentTimeMillis() - startMillis;
        double durationSeconds = Math.round((durationMillis / 1000.0) * 100.0) / 100.0;

        BenchmarkRunResult runResult = compileRunResult(candidateAgi, testDef, participant, durationSeconds, null, null);
        BenchmarkResultsStore.recordResult(catalog, runResult);
        return runResult;
    }

    /**
     * Uploads the recorded benchmark demonstration video to YouTube with metadata, description, and thumbnail.
     *
     * @param testDef The test definition.
     * @param participant The participant descriptor.
     * @param session The recorded video session.
     * @return The uploaded YouTube video URL, or {@code null} if authentication is missing.
     */
    private String uploadBenchmarkVideoToYouTube(TestDefinition testDef, BenchmarkParticipant participant, RecordedSession session) {
        try {
            YouTubeCredentials creds = YouTubeCredentials.load();
            if (!creds.isAuthenticated()) {
                log("YouTube is not authenticated. Video saved locally at: " + session.videoPath());
                return null;
            }

            String title = "⚡ Anahata-AGI-1: " + participant.modelId() + " on " + testDef.testCode() + " (" + testDef.title() + ")";
            String testUrlCode = testDef.testCode().toLowerCase().replace('_', '-');
            String description = "⚡ Anahata-AGI-1 Benchmark Run: " + testDef.testCode() + "\n"
                    + "--------------------------------------------------\n"
                    + "Model: " + participant.modelId() + "\n"
                    + "Provider: " + participant.providerUuid() + "\n"
                    + "Challenge: " + testDef.title() + "\n"
                    + "Duration: " + session.durationSeconds() + "s\n\n"
                    + "📊 Interactive Telemetry & Leaderboard:\n"
                    + "https://asi.anahata.uno/benchmarks/anahata-agi-1/" + testUrlCode + ".html\n\n"
                    + "🏆 Master Suite Leaderboard:\n"
                    + "https://asi.anahata.uno/benchmarks/anahata-agi-1/index.html\n\n"
                    + "Prompt:\n\"" + testDef.rawPrompt() + "\"\n\n"
                    + "#AnahataASI #Java #AI #Benchmarks #LLM #OpenSource #ForcaBarca";

            List<String> tags = List.of("AnahataASI", "Java", "AI", "Benchmarks", "LLM", testDef.testCode());

            YouTube youtube = getAgi().getToolkit(YouTube.class).orElse(new YouTube());
            YouTubeVideoUploadRequest request = YouTubeVideoUploadRequest.builder()
                    .videoFilePath(session.videoPath().toString())
                    .title(title)
                    .description(description)
                    .tags(tags)
                    .playlistId(creds.playlistId())
                    .privacyStatus("unlisted")
                    .build();

            String videoUrl = youtube.uploadVideo(request);
            log("YouTube video published: " + videoUrl);

            if (session.thumbnailPath() != null) {
                try {
                    String videoId = videoUrl.substring(videoUrl.lastIndexOf('/') + 1);
                    youtube.setThumbnail(videoId, session.thumbnailPath().toString());
                    log("Custom thumbnail set for video: " + videoId);
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
                .judgeScores(new HashMap<>())
                .videoUrl(videoUrl)
                .screenshotPath(screenshotPath)
                .sessionId(candidateAgi.getConfig().getSessionId())
                .observations(observations.toString().trim())
                .build();
    }
}
