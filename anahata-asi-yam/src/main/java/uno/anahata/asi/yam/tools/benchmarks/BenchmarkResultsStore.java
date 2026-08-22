/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.benchmarks;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import lombok.extern.slf4j.Slf4j;

/**
 * Generic persistence store and manager for JSON-formatted benchmark run telemetry and scorecards.
 * <p>
 * Provides decoupled reading, updating, and saving of {@link BenchmarkRunResult} records to any
 * designated JSON file path on disk without suite-specific hardcoding.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class BenchmarkResultsStore {

    /**
     * Shared JSON object mapper configured for clean field-only persistence, ISO-8601 timestamps,
     * and ignoring JavaBean getters or unknown properties.
     */
    private static final ObjectMapper MAPPER = new ObjectMapper()
            .registerModule(new JavaTimeModule())
            .enable(SerializationFeature.INDENT_OUTPUT)
            .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS)
            .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
            .setVisibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY)
            .setVisibility(PropertyAccessor.GETTER, JsonAutoDetect.Visibility.NONE)
            .setVisibility(PropertyAccessor.IS_GETTER, JsonAutoDetect.Visibility.NONE);

    /**
     * Loads all recorded benchmark runs from a specific JSON file.
     *
     * @param resultsFile The path to the JSON results file.
     * @return An unmodifiable list of previous test runs, or an empty list if file doesn't exist or is empty.
     */
    public static List<BenchmarkRunResult> loadResults(Path resultsFile) {
        if (resultsFile == null || !Files.exists(resultsFile)) {
            return Collections.emptyList();
        }

        try {
            byte[] data = Files.readAllBytes(resultsFile);
            if (data.length == 0) {
                return Collections.emptyList();
            }
            List<BenchmarkRunResult> list = MAPPER.readValue(data, new TypeReference<List<BenchmarkRunResult>>() {});
            return list != null ? list : Collections.emptyList();
        } catch (Exception e) {
            log.error("Failed to load benchmark results from {}", resultsFile, e);
            return Collections.emptyList();
        }
    }

    /**
     * Loads all recorded benchmark runs for a given test code within a catalog context.
     *
     * @param catalog The catalog context.
     * @param testCode The unique test identifier code.
     * @return List of previous test runs.
     */
    public static List<BenchmarkRunResult> loadResults(TestCatalog catalog, String testCode) {
        if (catalog == null) {
            return Collections.emptyList();
        }
        return loadResults(catalog.getResultsFileForTest(testCode));
    }

    /**
     * Appends a new benchmark run result to a specific results file and saves it atomically.
     *
     * @param resultsFile The path to the JSON results file.
     * @param result The benchmark run result to record.
     * @throws IOException If writing to disk fails.
     */
    public static synchronized void recordResult(Path resultsFile, BenchmarkRunResult result) throws IOException {
        if (resultsFile == null) {
            throw new IllegalArgumentException("Results file path cannot be null");
        }

        List<BenchmarkRunResult> existing = new ArrayList<>(loadResults(resultsFile));
        existing.add(result);

        Files.createDirectories(resultsFile.getParent());
        MAPPER.writeValue(resultsFile.toFile(), existing);
        log.info("Recorded benchmark result for {} ({}) to {}", result.participant().modelId(), result.testCode(), resultsFile);
    }

    /**
     * Appends a new benchmark run result to the results file defined by the given catalog.
     *
     * @param catalog The catalog context.
     * @param result The benchmark run result to record.
     * @throws IOException If writing to disk fails.
     */
    public static synchronized void recordResult(TestCatalog catalog, BenchmarkRunResult result) throws IOException {
        if (catalog == null) {
            throw new IllegalArgumentException("Catalog cannot be null");
        }
        recordResult(catalog.getResultsFileForTest(result.testCode()), result);
    }

    /**
     * Adds or updates a judge's score for a specific run in a results file.
     *
     * @param resultsFile The path to the JSON results file.
     * @param participant The composite candidate participant key (providerUuid, modelId, thinkingLevel).
     * @param judgeName The name of the judge (e.g. "Pablo", "Vijay").
     * @param score The score given by the judge.
     * @return {@code true} if a matching run was found and updated, {@code false} otherwise.
     * @throws IOException If saving fails.
     */
    public static synchronized boolean submitJudgeScore(Path resultsFile, BenchmarkParticipant participant, String judgeName, double score) throws IOException {
        if (resultsFile == null || !Files.exists(resultsFile)) {
            return false;
        }

        List<BenchmarkRunResult> runs = new ArrayList<>(loadResults(resultsFile));
        boolean found = false;

        for (int i = 0; i < runs.size(); i++) {
            BenchmarkRunResult run = runs.get(i);
            if (run.participant().equals(participant)) {
                var updatedScores = new HashMap<>(run.judgeScores());
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
            MAPPER.writeValue(resultsFile.toFile(), runs);
            log.info("Updated judge score for {} by {}: {} in {}", participant, judgeName, score, resultsFile);
        }
        return found;
    }

    /**
     * Adds or updates a judge's score for a specific run in a catalog.
     *
     * @param catalog The catalog context.
     * @param testCode The test code.
     * @param participant The candidate participant key.
     * @param judgeName The judge name.
     * @param score The score.
     * @return {@code true} if updated, {@code false} otherwise.
     * @throws IOException If saving fails.
     */
    public static synchronized boolean submitJudgeScore(TestCatalog catalog, String testCode, BenchmarkParticipant participant, String judgeName, double score) throws IOException {
        if (catalog == null) {
            return false;
        }
        return submitJudgeScore(catalog.getResultsFileForTest(testCode), participant, judgeName, score);
    }
}
