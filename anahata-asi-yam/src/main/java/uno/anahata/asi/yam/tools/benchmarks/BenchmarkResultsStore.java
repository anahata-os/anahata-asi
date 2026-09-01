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
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
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
     * @throws IllegalStateException If a run with the same session ID already exists.
     */
    public static synchronized void recordResult(Path resultsFile, BenchmarkRunResult result) throws IOException {
        if (resultsFile == null) {
            throw new IllegalArgumentException("Results file path cannot be null");
        }

        List<BenchmarkRunResult> existing = new ArrayList<>(loadResults(resultsFile));
        if (result.sessionId() != null && !result.sessionId().isBlank()) {
            boolean duplicate = existing.stream()
                    .anyMatch(run -> run.sessionId() != null && run.sessionId().equals(result.sessionId()));
            if (duplicate) {
                throw new IllegalStateException("A benchmark run with session ID " + result.sessionId() + " already exists in " + resultsFile);
            }
        }
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
        refreshWebsiteManifests(catalog);
    }

    /**
     * Adds or updates a judge's score for a specific run in a results file, keyed by session ID.
     *
     * @param resultsFile The path to the JSON results file.
     * @param sessionId The unique session ID of the run to score.
     * @param judgeScore The judge score DTO carrying name, score, and optional comments.
     * @return {@code true} if a matching run was found and updated, {@code false} otherwise.
     * @throws IOException If saving fails.
     */
    public static synchronized boolean submitJudgeScore(Path resultsFile, String sessionId, JudgeScore judgeScore) throws IOException {
        if (resultsFile == null || !Files.exists(resultsFile)) {
            return false;
        }

        List<BenchmarkRunResult> runs = new ArrayList<>(loadResults(resultsFile));
        boolean found = false;

        for (int i = 0; i < runs.size(); i++) {
            BenchmarkRunResult run = runs.get(i);
            if (run.sessionId() != null && run.sessionId().equals(sessionId)) {
                var updatedScores = new ArrayList<>(run.judgeScores());
                updatedScores.removeIf(judge -> judge != null && judge.name() != null && judge.name().equals(judgeScore.name()));
                updatedScores.add(judgeScore);

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
            log.info("Updated judge score for session {} by {}: {} in {}", sessionId, judgeScore.name(), judgeScore.score(), resultsFile);
        }
        return found;
    }

    /**
     * Adds or updates a judge's score for a specific run in a catalog, keyed by session ID.
     *
     * @param catalog The catalog context.
     * @param testCode The test code.
     * @param sessionId The unique session ID of the run to score.
     * @param judgeScore The judge score DTO.
     * @return {@code true} if updated, {@code false} otherwise.
     * @throws IOException If saving fails.
     */
    public static synchronized boolean submitJudgeScore(TestCatalog catalog, String testCode, String sessionId, JudgeScore judgeScore) throws IOException {
        if (catalog == null) {
            return false;
        }
        return submitJudgeScore(catalog.getResultsFileForTest(testCode), sessionId, judgeScore);
    }

    /**
     * Filters recorded benchmark runs from a specific JSON results file, applying AND semantics
     * across all non-null predicate filters.
     *
     * @param resultsFile The path to the JSON results file.
     * @param providerUuid Optional provider UUID filter (case-insensitive).
     * @param modelId Optional model ID filter (case-insensitive).
     * @param passed Optional pass/fail status filter.
     * @param sessionId Optional session ID filter (case-insensitive).
     * @return The list of runs matching every provided filter, or an empty list if none match.
     */
    public static List<BenchmarkRunResult> findResults(Path resultsFile, String providerUuid, String modelId, Boolean passed, String sessionId) {
        List<BenchmarkRunResult> matches = new ArrayList<>();
        for (BenchmarkRunResult run : loadResults(resultsFile)) {
            if (providerUuid != null && !providerUuid.isBlank() && !providerUuid.equalsIgnoreCase(run.participant().providerUuid())) {
                continue;
            }
            if (modelId != null && !modelId.isBlank() && !modelId.equalsIgnoreCase(run.participant().modelId())) {
                continue;
            }
            if (passed != null && !passed.equals(run.passed())) {
                continue;
            }
            if (sessionId != null && !sessionId.isBlank() && !sessionId.equalsIgnoreCase(run.sessionId())) {
                continue;
            }
            matches.add(run);
        }
        return matches;
    }

    /**
     * Replaces an entire recorded benchmark run in the results file, matching by session ID (the unique primary key).
     *
     * @param resultsFile The path to the JSON results file.
     * @param updated The fully populated replacement {@link BenchmarkRunResult}.
     * @return {@code true} if a matching run was found and replaced, {@code false} otherwise.
     * @throws IOException If saving fails.
     * @throws IllegalArgumentException If the updated record lacks a session ID.
     */
    public static synchronized boolean updateResult(Path resultsFile, BenchmarkRunResult updated) throws IOException {
        if (resultsFile == null || !Files.exists(resultsFile)) {
            return false;
        }
        String targetSessionId = updated.sessionId();
        if (targetSessionId == null || targetSessionId.isBlank()) {
            throw new IllegalArgumentException("Cannot update a benchmark result without a session ID.");
        }
        List<BenchmarkRunResult> runs = new ArrayList<>(loadResults(resultsFile));
        boolean found = false;
        for (int i = 0; i < runs.size(); i++) {
            BenchmarkRunResult run = runs.get(i);
            if (run.sessionId() != null && run.sessionId().equals(targetSessionId)) {
                runs.set(i, updated);
                found = true;
                break;
            }
        }
        if (found) {
            MAPPER.writeValue(resultsFile.toFile(), runs);
            log.info("Updated benchmark result for session {} ({})", updated.sessionId(), updated.testCode());
        }
        return found;
    }

    /**
     * Writes the machine-readable catalog manifest ({@code catalog.json}) describing the suite and its tests.
     * <p>
     * Emits the catalog id, name, description, and every registered test's code, title, and raw prompt
     * so the static website renders the correct suite without hardcoded HTML.
     * </p>
     *
     * @param catalog The catalog context.
     * @throws IOException If writing to disk fails.
     */
    public static synchronized void writeCatalogManifest(TestCatalog catalog) throws IOException {
        if (catalog == null || catalog.getResultsDirectory() == null) {
            return;
        }
        Path dir = catalog.getResultsDirectory();
        Files.createDirectories(dir);

        ObjectNode root = MAPPER.createObjectNode();
        root.put("id", catalog.getId());
        root.put("name", catalog.getName());
        root.put("description", catalog.getDescription());

        ArrayNode tests = root.putArray("tests");
        for (TestDefinition test : catalog.getTests()) {
            ObjectNode t = tests.addObject();
            t.put("testCode", test.testCode());
            t.put("title", test.title());
            t.put("rawPrompt", test.rawPrompt() != null ? test.rawPrompt() : "");
        }

        MAPPER.writeValue(dir.resolve("catalog.json").toFile(), root);
        log.info("Wrote benchmark catalog manifest to {}", dir.resolve("catalog.json"));
    }

    /**
     * Writes the combined results file ({@code results.json}) aggregating every run across all tests in a catalog.
     * <p>
     * Flattens each test's per-run scorecard into a single ordered list so the public leaderboard
     * can render and filter everything from one fetch.
     * </p>
     *
     * @param catalog The catalog context.
     * @throws IOException If writing to disk fails.
     */
    public static synchronized void writeCombinedResults(TestCatalog catalog) throws IOException {
        if (catalog == null || catalog.getResultsDirectory() == null) {
            return;
        }
        Path dir = catalog.getResultsDirectory();
        Files.createDirectories(dir);

        List<BenchmarkRunResult> all = new ArrayList<>();
        for (TestDefinition test : catalog.getTests()) {
            all.addAll(loadResults(catalog, test.testCode()));
        }

        MAPPER.writeValue(dir.resolve("results.json").toFile(), all);
        log.info("Wrote combined benchmark results ({} runs) to {}", all.size(), dir.resolve("results.json"));
    }

    /**
     * Regenerates both public website artifacts ({@code catalog.json} and {@code results.json}) for a catalog.
     *
     * @param catalog The catalog context.
     * @throws IOException If writing to disk fails.
     */
    public static synchronized void refreshWebsiteManifests(TestCatalog catalog) throws IOException {
        writeCatalogManifest(catalog);
        writeCombinedResults(catalog);
    }
}
