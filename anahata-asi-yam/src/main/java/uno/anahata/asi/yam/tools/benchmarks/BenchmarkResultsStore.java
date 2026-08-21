/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.benchmarks;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;

/**
 * Persistence store and manager for JSON-formatted benchmark run telemetry and scorecards.
 * <p>
 * Manages reading, updating, and saving {@link BenchmarkRunResult} records in test-specific
 * JSON files (e.g. {@code java-jna-1-results.json}) located within the website directory
 * ({@code anahata-asi-web/src/main/resources/web/benchmarks/anahata-agi-1/}).
 * </p>
 * <p>
 * Automatically resolves the workspace development repository path with fallback to
 * the container's working directory.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class BenchmarkResultsStore {

    /**
     * Shared JSON object mapper configured for clean indentation and ISO-8601 timestamps.
     */
    private static final ObjectMapper MAPPER = new ObjectMapper()
            .registerModule(new JavaTimeModule())
            .enable(SerializationFeature.INDENT_OUTPUT)
            .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);

    /**
     * Resolves the primary directory where benchmark JSON result files are stored.
     *
     * @return The path to the benchmark results directory.
     */
    public static Path getResultsDirectory() {
        // 1. Try resolving within the source tree of anahata-asi-web
        Path devWebPath = Paths.get(System.getProperty("user.home"), "NetBeansProjects", "anahata-asi-parent",
                "anahata-asi-web", "src", "main", "resources", "web", "benchmarks", "anahata-agi-1");
        if (Files.exists(devWebPath)) {
            return devWebPath;
        }

        // 2. Try resolving relative to the current working directory
        Path relativeWebPath = Paths.get("anahata-asi-web", "src", "main", "resources", "web", "benchmarks", "anahata-agi-1");
        if (Files.exists(relativeWebPath)) {
            return relativeWebPath;
        }

        // 3. Fallback to ~/.anahata/asi/benchmarks/anahata-agi-1
        Path fallbackPath = AbstractAsiContainer.getWorkDirSubDir("benchmarks").resolve("anahata-agi-1");
        try {
            Files.createDirectories(fallbackPath);
        } catch (IOException e) {
            log.error("Could not create fallback benchmark directory: {}", fallbackPath, e);
        }
        return fallbackPath;
    }

    /**
     * Resolves the JSON file path for a specific benchmark test code.
     * <p>
     * Converts test codes such as {@code "JAVA-JNA-1"} into {@code "java-jna-1-results.json"}.
     * </p>
     *
     * @param testCode The test identifier code.
     * @return The path to the test's JSON results file.
     */
    public static Path getResultsFileForTest(String testCode) {
        String filename = testCode.toLowerCase().replace('_', '-') + "-results.json";
        return getResultsDirectory().resolve(filename);
    }

    /**
     * Loads all recorded benchmark runs for a given test code.
     *
     * @param testCode The unique test identifier code.
     * @return An unmodifiable list of previous test runs, or an empty list if no runs exist.
     */
    public static List<BenchmarkRunResult> loadResults(String testCode) {
        Path file = getResultsFileForTest(testCode);
        if (!Files.exists(file)) {
            return Collections.emptyList();
        }

        try {
            byte[] data = Files.readAllBytes(file);
            if (data.length == 0) {
                return Collections.emptyList();
            }
            List<BenchmarkRunResult> list = MAPPER.readValue(data, new TypeReference<List<BenchmarkRunResult>>() {});
            return list != null ? list : Collections.emptyList();
        } catch (Exception e) {
            log.error("Failed to load benchmark results from {}", file, e);
            return Collections.emptyList();
        }
    }

    /**
     * Appends a new benchmark run result to the test's results file and saves it.
     *
     * @param result The benchmark run result to record.
     * @throws IOException If writing to disk fails.
     */
    public static synchronized void recordResult(BenchmarkRunResult result) throws IOException {
        String testCode = result.testCode();
        Path file = getResultsFileForTest(testCode);

        List<BenchmarkRunResult> existing = new ArrayList<>(loadResults(testCode));
        existing.add(result);

        Files.createDirectories(file.getParent());
        MAPPER.writeValue(file.toFile(), existing);
        log.info("Recorded benchmark result for {} ({}) to {}", result.participant().modelId(), testCode, file);
    }

    /**
     * Adds or updates a judge's score for a specific run identified by the candidate {@link BenchmarkParticipant}.
     *
     * @param testCode The test code (e.g. "JAVA-JNA-1").
     * @param participant The composite candidate participant key (providerUuid, modelId, thinkingLevel).
     * @param judgeName The name of the judge (e.g. "Pablo", "Vijay").
     * @param score The score given by the judge.
     * @return {@code true} if a matching run was found and updated, {@code false} otherwise.
     * @throws IOException If saving fails.
     */
    public static synchronized boolean submitJudgeScore(String testCode, BenchmarkParticipant participant, String judgeName, double score) throws IOException {
        Path file = getResultsFileForTest(testCode);
        List<BenchmarkRunResult> runs = new ArrayList<>(loadResults(testCode));
        boolean found = false;

        for (int i = 0; i < runs.size(); i++) {
            BenchmarkRunResult run = runs.get(i);
            if (run.participant().equals(participant)) {
                var updatedScores = new java.util.HashMap<>(run.judgeScores());
                updatedScores.put(judgeName, score);

                BenchmarkRunResult updatedRun = BenchmarkRunResult.builder()
                        .participant(run.participant())
                        .testCode(run.testCode())
                        .asiContainer(run.asiContainer())
                        .timestamp(run.timestamp())
                        .durationSeconds(run.durationSeconds())
                        .turns(run.turns())
                        .promptTokens(run.promptTokens())
                        .candidatesTokens(run.candidatesTokens())
                        .thoughtsTokens(run.thoughtsTokens())
                        .totalTokens(run.totalTokens())
                        .passed(run.passed())
                        .judgeScores(updatedScores)
                        .videoUrl(run.videoUrl())
                        .screenshotPath(run.screenshotPath())
                        .sessionId(run.sessionId())
                        .observations(run.observations())
                        .build();

                runs.set(i, updatedRun);
                found = true;
                break;
            }
        }

        if (found) {
            MAPPER.writeValue(file.toFile(), runs);
            log.info("Updated judge score for {} on {} by {}: {}", participant, testCode, judgeName, score);
        }
        return found;
    }

    /**
     * Convenience method to submit a judge score using raw participant parameters.
     *
     * @param testCode The test code.
     * @param providerUuid The AI provider UUID.
     * @param modelId The model identifier.
     * @param thinkingLevel The thinking level.
     * @param judgeName The name of the judge.
     * @param score The score.
     * @return {@code true} if updated, {@code false} otherwise.
     * @throws IOException If saving fails.
     */
    public static synchronized boolean submitJudgeScore(String testCode, String providerUuid, String modelId, uno.anahata.asi.agi.provider.ThinkingLevel thinkingLevel, String judgeName, double score) throws IOException {
        return submitJudgeScore(testCode, BenchmarkParticipant.of(providerUuid, modelId, thinkingLevel), judgeName, score);
    }
}
