/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.benchmarks;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import lombok.Getter;
import lombok.Setter;

/**
 * Abstract base catalog representing an extensible benchmark suite and repository of test definitions.
 * <p>
 * Encapsulates the suite identifier, display name, description, standard prompt header/footer templates,
 * customizable results storage directory, and registered {@link TestDefinition}s.
 * </p>
 *
 * @author anahata
 */
@Getter
@Setter
public abstract class TestCatalog {

    /**
     * The unique identifier code for the catalog (e.g., "ANAHATA-AGI-1").
     */
    private String id;

    /**
     * The human-readable name of the catalog.
     */
    private String name;

    /**
     * The detailed description of the suite's goals and scope.
     */
    private String description;

    /**
     * The standard header prepended to all test prompts in this catalog.
     */
    private String standardHeader;

    /**
     * The standard footer appended to all test prompts in this catalog.
     */
    private String standardFooter;

    /**
     * The customizable filesystem directory where JSON results files are persisted.
     */
    private Path resultsDirectory;

    /**
     * The list of registered test definitions in this catalog.
     */
    private final List<TestDefinition> tests = new ArrayList<>();

    /**
     * Base constructor for a benchmark test catalog.
     *
     * @param id The unique catalog identifier.
     * @param name The human-readable catalog name.
     * @param description The catalog description.
     * @param standardHeader The standard prompt header template.
     * @param standardFooter The standard prompt footer template.
     * @param resultsDirectory The filesystem directory where results are stored.
     */
    public TestCatalog(String id, String name, String description, String standardHeader, String standardFooter, Path resultsDirectory) {
        this.id = id;
        this.name = name;
        this.description = description;
        this.standardHeader = standardHeader;
        this.standardFooter = standardFooter;
        this.resultsDirectory = resultsDirectory;
    }

    /**
     * Retrieves an unmodifiable view of all registered test definitions.
     *
     * @return List of test definitions.
     */
    public List<TestDefinition> getTests() {
        return Collections.unmodifiableList(tests);
    }

    /**
     * Adds a test definition to this catalog.
     *
     * @param test The test definition to register.
     */
    public void addTest(TestDefinition test) {
        if (test != null) {
            tests.add(test);
        }
    }

    /**
     * Finds a test definition by its unique test code (case-insensitive).
     *
     * @param testCode The test code to look up.
     * @return An Optional containing the matching test definition if found.
     */
    public Optional<TestDefinition> findByCode(String testCode) {
        if (testCode == null || testCode.isBlank()) {
            return Optional.empty();
        }
        return tests.stream()
                .filter(test -> test.testCode().equalsIgnoreCase(testCode.trim()))
                .findFirst();
    }

    /**
     * Formats the full prompt for a test by applying this catalog's standard header and footer templates.
     *
     * @param test The test definition.
     * @return The formatted prompt ready for submission to the candidate AGI.
     */
    public String formatPrompt(TestDefinition test) {
        StringBuilder sb = new StringBuilder();
        if (standardHeader != null && !standardHeader.isBlank()) {
            sb.append(String.format(standardHeader, test.testCode(), test.title())).append("\n\n");
        }
        sb.append(test.rawPrompt());
        if (standardFooter != null && !standardFooter.isBlank()) {
            sb.append("\n\n").append(standardFooter);
        }
        return sb.toString();
    }

    /**
     * Resolves the JSON scorecard path for a specific test code within this catalog's results directory.
     *
     * @param testCode The test code (e.g. "JAVA-JNA-1").
     * @return The path to the JSON results file.
     */
    public Path getResultsFileForTest(String testCode) {
        String filename = testCode.toLowerCase().replace('_', '-') + "-results.json";
        return resultsDirectory != null ? resultsDirectory.resolve(filename) : null;
    }

    /**
     * Formats this catalog and its test definitions into a clean Markdown representation
     * suitable for RAG message prompt augmentation.
     *
     * @return The Markdown string describing this catalog.
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("### 🏆 Catalog: ").append(name).append(" (`").append(id).append("`)\n");
        if (description != null && !description.isBlank()) {
            sb.append("- **Description**: ").append(description).append("\n");
        }
        sb.append("- **Results Directory**: `").append(resultsDirectory != null ? resultsDirectory.toAbsolutePath() : "Default").append("`\n");
        sb.append("- **Active Tests**: ").append(tests.size()).append("\n\n");

        for (TestDefinition test : tests) {
            sb.append("#### ⚡ `").append(test.testCode()).append("`: ").append(test.title()).append("\n");
            sb.append("- **Toolkits**: ").append(String.join(", ", test.getToolkitFqns())).append("\n");
            sb.append("- **Raw Prompt**: \"").append(test.rawPrompt()).append("\"\n");

            List<BenchmarkRunResult> results = BenchmarkResultsStore.loadResults(this, test.testCode());
            if (!results.isEmpty()) {
                sb.append("- **Recorded Runs (").append(results.size()).append(")**:\n");
                for (BenchmarkRunResult r : results) {
                    Double avgScore = r.getAverageScore();
                    String scoreStr = avgScore != null ? String.format("%.1f", avgScore) : "Unrated";
                    sb.append("  - **").append(r.participant().modelId()).append("** (Provider: `").append(r.participant().providerUuid()).append("`)")
                      .append(" — Duration: ").append(r.durationSeconds()).append("s, Turns: ").append(r.turns())
                      .append(", Tokens: ").append(r.totalTokens())
                      .append(", Status: ").append(r.passed() ? "✅ PASSED" : "❌ FAILED")
                      .append(", Avg Score: ").append(scoreStr);
                    if (r.videoUrl() != null && !r.videoUrl().isBlank()) {
                        sb.append(", Video: ").append(r.videoUrl());
                    }
                    sb.append("\n");
                }
            } else {
                sb.append("- **Recorded Runs**: None yet (Pending first test execution)\n");
            }
            sb.append("\n");
        }
        return sb.toString().trim();
    }
}
