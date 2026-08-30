/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.benchmarks;

import lombok.Builder;

/**
 * Immutable descriptor of a single judge's subjective evaluation of a benchmark run.
 * <p>
 * Encapsulates the judge's display name, numeric score, and optional free-form comments
 * into one reusable unit that composes into a run's {@link BenchmarkRunResult#judgeScores()} list.
 * </p>
 *
 * @param name The display name of the judge (e.g. {@code "Pablo"}, {@code "Vijay"}).
 * @param score The numeric score awarded by this judge (e.g. {@code 9.5}).
 * @param comments Optional free-form qualitative feedback from the judge.
 *
 * @author anahata
 */
@Builder
public record JudgeScore(
        String name,
        double score,
        String comments
) {
}
