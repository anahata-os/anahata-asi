/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.desktop.tools.benchmarks;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.Builder;
import uno.anahata.asi.agi.tool.ToolPermission;

/**
 * Immutable descriptor representing a standardized test challenge within the Anahata-AGI-1 benchmark suite.
 * <p>
 * Encapsulates the unique test code, human-readable title, raw goal prompt, list of {@link ToolkitSettings},
 * and execution timeout.
 * </p>
 *
 * @param testCode The unique identifier code for the test (e.g., "JAVA-JNA-1").
 * @param title The descriptive title of the challenge.
 * @param rawPrompt The core task objective delivered to candidate models.
 * @param toolkits The list of {@link ToolkitSettings} configuring enabled toolkits and their permissions.
 * @param timeoutSeconds The maximum allotted execution duration before timeout in seconds.
 * 
 * @author anahata
 */
@Builder
public record Agi1TestDefinition(
        String testCode,
        String title,
        String rawPrompt,
        List<ToolkitSettings> toolkits,
        int timeoutSeconds
) {

    /**
     * The standard header prepended to all official benchmark prompts.
     */
    public static final String STANDARD_HEADER = 
            "You are participating in the official Anahata-AGI-1 Benchmark (%s: %s).\n"
            + "Your task must be executed autonomously with zero defects.";

    /**
     * The standard footer appended to all official benchmark prompts.
     */
    public static final String STANDARD_FOOTER = 
            "Do not seek user prompt, confirmation, or follow-up. Execute all necessary tools "
            + "and continue until your task has been completely fulfilled.";

    /**
     * Canonical constructor providing unmodifiable collection copies.
     *
     * @param testCode The unique identifier code.
     * @param title The challenge title.
     * @param rawPrompt The raw prompt text.
     * @param toolkits The list of toolkit settings.
     * @param timeoutSeconds Timeout in seconds.
     */
    public Agi1TestDefinition {
        toolkits = toolkits != null ? Collections.unmodifiableList(toolkits) : Collections.emptyList();
    }

    /**
     * Extracts the fully qualified class names (FQNs) of all configured toolkits for this test.
     *
     * @return List of toolkit class FQNs.
     */
    public List<String> getToolkitFqns() {
        return toolkits.stream()
                .map(ts -> ts.toolkitClass().getName())
                .toList();
    }

    /**
     * Aggregates and resolves all tool permission overrides configured across all toolkits.
     *
     * @return A consolidated map of tool permission keys (e.g. {@code "SwingJava.compileAndExecute"}) to their permissions.
     */
    public Map<String, ToolPermission> getResolvedToolPermissions() {
        Map<String, ToolPermission> resolved = new HashMap<>();
        for (ToolkitSettings ts : toolkits) {
            resolved.putAll(ts.getResolvedPermissions());
        }
        return Collections.unmodifiableMap(resolved);
    }

    /**
     * Assembles the full, standardized prompt by wrapping the raw prompt with the official
     * header and autonomous completion footer.
     *
     * @return The formatted prompt ready for submission to the candidate AGI.
     */
    public String getAssembledPrompt() {
        return String.format(STANDARD_HEADER, testCode, title)
                + "\n\n"
                + rawPrompt
                + "\n\n"
                + STANDARD_FOOTER;
    }
}
