/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.benchmarks;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.AgiUserMessage;
import uno.anahata.asi.agi.message.ResponseUsageMetadata;
import uno.anahata.asi.agi.provider.Response;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.toolkit.java.Java;

/**
 * Programmatic benchmark orchestrator and execution engine for the Anahata-AGI-1 suite.
 * <p>
 * Allows orchestrating autonomous benchmark runs against candidate AI models under strict,
 * standardized environments. Spawns isolated child {@link Agi} sessions, automatically applies
 * toolkits and permissions, aggregates exact multi-turn token metrics, evaluates pass/fail status,
 * and persists results directly to the website JSON store.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Programmatic benchmark orchestrator for the Anahata-AGI-1 suite.")
public class Agi1Benchmark extends AnahataToolkit {

    /**
     * Executes the JAVA-JNA-1 benchmark test (OS Hardware &amp; System Values Dashboard).
     *
     * @param participant The candidate participant descriptor (provider UUID, model ID, thinking level).
     * @param openSession Whether to open the child AGI session tab in the UI.
     * @return The telemetry record of the benchmark run.
     * @throws Exception If benchmark orchestration fails.
     */
    @AgiTool("Runs the official JAVA-JNA-1 benchmark test (OS Hardware & System Values Dashboard).")
    public BenchmarkRunResult testJna1(
            @AgiToolParam("The candidate participant descriptor (provider UUID, model ID, thinking level).") BenchmarkParticipant participant,
            @AgiToolParam("Whether to open the child session tab in the UI during execution.") boolean openSession) throws Exception {
        return runTest("JAVA-JNA-1", participant, openSession);
    }

    /**
     * Executes the JAVA-ARKANOID-1 benchmark test (Retro Arcade Game Execution).
     *
     * @param participant The candidate participant descriptor.
     * @param openSession Whether to open the child AGI session tab in the UI.
     * @return The telemetry record of the benchmark run.
     * @throws Exception If benchmark orchestration fails.
     */
    @AgiTool("Runs the official JAVA-ARKANOID-1 benchmark test (Retro Arcade Game Execution).")
    public BenchmarkRunResult testArkanoid1(
            @AgiToolParam("The candidate participant descriptor.") BenchmarkParticipant participant,
            @AgiToolParam("Whether to open the child session tab in the UI.") boolean openSession) throws Exception {
        return runTest("JAVA-ARKANOID-1", participant, openSession);
    }

    /**
     * Executes any registered benchmark test from the catalog by its test code.
     *
     * @param testCode The test code (e.g., "JAVA-JNA-1", "JAVA-ARKANOID-1").
     * @param participant The candidate participant descriptor.
     * @param openSession Whether to open the child AGI session tab in the UI.
     * @return The telemetry record of the benchmark run.
     * @throws Exception If benchmark orchestration fails or test code is unknown.
     */
    @AgiTool("Runs a specific benchmark test from the Anahata-AGI-1 catalog.")
    public BenchmarkRunResult runTest(
            @AgiToolParam("The test code from the catalog (e.g., 'JAVA-JNA-1', 'JAVA-ARKANOID-1').") String testCode,
            @AgiToolParam("The candidate participant descriptor.") BenchmarkParticipant participant,
            @AgiToolParam("Whether to open the child session tab in the UI.") boolean openSession) throws Exception {
        Agi1TestDefinition testDef = Agi1Catalog.findByCode(testCode)
                .orElseThrow(() -> new AgiToolException("Unknown benchmark test code: " + testCode));

        return executeBenchmark(testDef, participant, openSession);
    }

    /**
     * Sequentially executes all registered benchmark tests in the catalog for a given model.
     *
     * @param participant The candidate participant descriptor.
     * @param openSession Whether to open the child AGI session tabs in the UI.
     * @return A list of telemetry records for all executed tests.
     * @throws Exception If any benchmark execution fails.
     */
    @AgiTool("Sequentially executes the complete Anahata-AGI-1 benchmark suite for a candidate model.")
    public List<BenchmarkRunResult> runAll(
            @AgiToolParam("The candidate participant descriptor.") BenchmarkParticipant participant,
            @AgiToolParam("Whether to open child session tabs in the UI.") boolean openSession) throws Exception {
        List<BenchmarkRunResult> results = new ArrayList<>();

        for (Agi1TestDefinition testDef : Agi1Catalog.getAllTests()) {
            log("Starting benchmark test: " + testDef.testCode() + " (" + testDef.title() + ")");
            BenchmarkRunResult result = executeBenchmark(testDef, participant, openSession);
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
    @AgiTool("Submits or updates a judge's score for a candidate run in the results database.")
    public String submitJudgeScore(
            @AgiToolParam("The test code (e.g., 'JAVA-JNA-1').") String testCode,
            @AgiToolParam("The candidate participant descriptor.") BenchmarkParticipant participant,
            @AgiToolParam("The judge name (e.g., 'Pablo', 'Vijay').") String judgeName,
            @AgiToolParam("The score (0.0 to 10.0 or 0 to 100).") double score) throws Exception {
        boolean updated = BenchmarkResultsStore.submitJudgeScore(testCode, participant, judgeName, score);
        if (updated) {
            return "Successfully recorded judge score of " + score + " by " + judgeName + " for " + participant + " on " + testCode;
        } else {
            return "No matching benchmark run found for " + participant + " on " + testCode + ". Execute the test first before scoring.";
        }
    }

    /**
     * Lists all recorded benchmark runs and scores for a specific test code.
     *
     * @param testCode The test code (e.g. "JAVA-JNA-1").
     * @return The list of recorded runs.
     * @throws Exception If reading the results store fails.
     */
    @AgiTool("Lists all recorded benchmark runs and scores for a specific test code.")
    public List<BenchmarkRunResult> listResults(
            @AgiToolParam("The test code (e.g., 'JAVA-JNA-1', 'JAVA-ARKANOID-1').") String testCode) throws Exception {
        return BenchmarkResultsStore.loadResults(testCode);
    }

    /**
     * Internal execution harness that provisions the child AGI, executes the test autonomously,
     * harvests fine-grained telemetry, and persists the result.
     *
     * @param testDef The test definition.
     * @param participant The candidate participant.
     * @param openSession Whether to open the session UI.
     * @return The complete benchmark run result.
     * @throws Exception If an unrecoverable execution error occurs.
     */
    private BenchmarkRunResult executeBenchmark(Agi1TestDefinition testDef, BenchmarkParticipant participant, boolean openSession) throws Exception {
        AbstractAsiContainer container = getAsiContainer();

        AgiConfig config = container.createNewAgiConfig();
        config.setSelectedProviderUuid(participant.providerUuid());
        config.setSelectedModelId(participant.modelId());
        config.setAutoReplyTools(true);
        config.setParentUuid(getAgi().getConfig().getSessionId());

        // Resolve concrete Java toolkit class running in this container (e.g. NbJava or SwingJava)
        Class<?> concreteJavaClass = getAgi().getToolkit(Java.class)
                .map(Object::getClass)
                .orElse(Java.class);

        // Isolated toolkits defined strictly by the test specification
        config.getToolClasses().clear();
        for (ToolkitSettings ts : testDef.toolkits()) {
            if (Java.class.isAssignableFrom(ts.toolkitClass())) {
                config.getToolClasses().add(concreteJavaClass);
            } else {
                config.getToolClasses().add(ts.toolkitClass());
            }
        }

        log("Spawning candidate AGI session for test: " + testDef.testCode() + " with model: " + participant.modelId());
        Agi candidateAgi = container.createNewAgi(config);
        candidateAgi.setNickname("Bench: " + testDef.testCode() + " - " + participant.modelId());
        candidateAgi.getRequestConfig().setThinkingLevel(participant.thinkingLevel());

        // Apply strict tool permission overrides with the concrete class resolved
        testDef.getResolvedToolPermissions(concreteJavaClass).forEach((toolName, permission) -> {
            candidateAgi.getToolManager().findToolByName(toolName)
                    .ifPresent(tool -> tool.setPermission(permission));
        });

        if (!openSession) {
            container.close(candidateAgi);
        }

        long startMillis = System.currentTimeMillis();
        String prompt = testDef.getAssembledPrompt();

        try {
            log("Submitting official benchmark prompt to candidate AGI: " + candidateAgi.getShortId());
            AgiUserMessage userMsg = new AgiUserMessage(candidateAgi, getAgi().getConfig().getSessionId());
            userMsg.addTextPart(prompt);
            candidateAgi.sendMessage(userMsg);
        } catch (Exception e) {
            log.error("Benchmark candidate execution failed with exception", e);
            error(e);
        }

        long durationMillis = System.currentTimeMillis() - startMillis;
        double durationSeconds = Math.round((durationMillis / 1000.0) * 100.0) / 100.0;

        // Harvest metrics across the complete conversation history
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

        BenchmarkRunResult runResult = BenchmarkRunResult.builder()
                .participant(participant)
                .testCode(testDef.testCode())
                .asiContainer(container.getClass().getSimpleName())
                .timestamp(Instant.now())
                .durationSeconds(durationSeconds)
                .turns(turns)
                .promptTokens(promptTokens)
                .candidatesTokens(candidatesTokens)
                .thoughtsTokens(thoughtsTokens)
                .totalTokens(totalTokens > 0 ? totalTokens : (promptTokens + candidatesTokens + thoughtsTokens))
                .passed(passed)
                .judgeScores(new HashMap<>())
                .sessionId(candidateAgi.getConfig().getSessionId())
                .observations(observations.toString().trim())
                .build();

        // Persist to results.json in website
        BenchmarkResultsStore.recordResult(runResult);

        log("Benchmark run completed: " + testDef.testCode() + " | Time: " + durationSeconds + "s | Turns: " + turns + " | Prompt Tokens: " + promptTokens + " | Candidate Tokens: " + candidatesTokens + " | Passed: " + passed);

        return runResult;
    }
}
