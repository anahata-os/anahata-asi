/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.benchmarks;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import lombok.Builder;

/**
 * Immutable telemetry record capturing the complete execution metrics and outcome of a benchmark test run.
 * <p>
 * Binds the candidate {@link BenchmarkParticipant}, test code, duration, turn count,
 * fine-grained token economics (prompt, candidate, thoughts, and total), execution pass/fail status,
 * media assets (video URL, screenshot), container identifier, and multi-judge subjective scores.
 * </p>
 *
 * @param participant The candidate model participant configuration.
 * @param testCode The unique code of the benchmark test (e.g., {@code "JAVA-JNA-1"}).
 * @param asiContainer The identifier of the ASI container where the test was executed (e.g. {@code "NetBeansAsiContainer"}, {@code "AsiDesktopAsiContainer"}).
 * @param timestamp The exact time the benchmark run was executed.
 * @param durationSeconds The wall-clock execution duration in seconds.
 * @param turns The total number of conversational turns taken by the model.
 * @param promptTokens The total input (prompt) tokens consumed across all turns.
 * @param candidatesTokens The total output (candidate) tokens generated across all turns.
 * @param thoughtsTokens The total internal reasoning (thinking) tokens generated across all turns.
 * @param totalTokens The total interaction tokens reported by the provider.
 * @param passed Whether the benchmark test completed successfully with zero defects.
 * @param judgeScores A list of {@link JudgeScore} evaluations submitted by individual judges.
 * @param videoUrl The URL to the run demonstration video (e.g. YouTube video).
 * @param screenshotPath The relative or absolute path to the captured execution screenshot.
 * @param sessionId The unique UUID of the child AGI session that executed the test.
 * @param observations Optional notes, tool outputs, or qualitative developer feedback.
 * 
 * @author anahata
 */
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public record BenchmarkRunResult(
        BenchmarkParticipant participant,
        String testCode,
        String asiContainer,
        Instant timestamp,
        double durationSeconds,
        int turns,
        int promptTokens,
        int candidatesTokens,
        int thoughtsTokens,
        int totalTokens,
        boolean passed,
        List<JudgeScore> judgeScores,
        String videoUrl,
        String screenshotPath,
        String sessionId,
        String observations
) {

    /**
     * Canonical constructor ensuring judge scores map is unmodifiable and never null.
     *
     * @param participant The participant.
     * @param testCode The test code.
     * @param asiContainer The container ID.
     * @param timestamp The timestamp.
     * @param durationSeconds The duration in seconds.
     * @param turns The turns count.
     * @param promptTokens Prompt tokens.
     * @param candidatesTokens Candidate tokens.
     * @param thoughtsTokens Thoughts tokens.
     * @param totalTokens Total tokens.
     * @param passed Passed flag.
     * @param judgeScores The list of judge scores.
     * @param videoUrl Video URL.
     * @param screenshotPath Screenshot path.
     * @param sessionId Session UUID.
     * @param observations Observations.
     */
    public BenchmarkRunResult {
        judgeScores = judgeScores != null ? new ArrayList<>(judgeScores) : new ArrayList<>();
    }

    /**
     * Calculates the arithmetic average of all submitted judge scores.
     *
     * @return The average judge score, or {@code null} if no judge scores have been recorded yet.
     */
    @JsonIgnore
    public Double getAverageScore() {
        if (judgeScores == null || judgeScores.isEmpty()) {
            return null;
        }
        return judgeScores.stream()
                .filter(judge -> judge != null)
                .mapToDouble(JudgeScore::score)
                .average()
                .orElse(0.0);
    }
}
