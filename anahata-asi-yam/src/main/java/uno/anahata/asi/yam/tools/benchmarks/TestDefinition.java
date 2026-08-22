/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.benchmarks;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.Builder;
import uno.anahata.asi.agi.tool.ToolPermission;

/**
 * Immutable descriptor representing a standardized test challenge within a benchmark suite.
 * <p>
 * Encapsulates the unique test code, human-readable title, raw goal prompt, and optional list of {@link ToolkitSettings}.
 * If {@code toolkits} is {@code null}, the test inherits the container's default tool classes and permissions.
 * </p>
 *
 * @param testCode The unique identifier code for the test (e.g., "JAVA-JNA-1").
 * @param title The descriptive title of the challenge.
 * @param rawPrompt The core task objective delivered to candidate models.
 * @param toolkits The optional list of {@link ToolkitSettings} configuring enabled toolkits and their permissions (null for container defaults).
 * 
 * @author anahata
 */
@Builder
public record TestDefinition(
        String testCode,
        String title,
        String rawPrompt,
        List<ToolkitSettings> toolkits
) {

    /**
     * Canonical constructor preserving null or unmodifiable collection copies.
     *
     * @param testCode The unique identifier code.
     * @param title The challenge title.
     * @param rawPrompt The raw prompt text.
     * @param toolkits The list of toolkit settings.
     */
    public TestDefinition {
        toolkits = toolkits != null ? Collections.unmodifiableList(toolkits) : null;
    }

    /**
     * Extracts the fully qualified class names (FQNs) or names of all configured toolkits for this test.
     *
     * @return List of toolkit names/FQNs or an empty list if using container defaults.
     */
    public List<String> getToolkitFqns() {
        if (toolkits == null || toolkits.isEmpty()) {
            return List.of("Container Defaults");
        }
        return toolkits.stream()
                .map(ToolkitSettings::toolkit)
                .toList();
    }

    /**
     * Aggregates and resolves all tool permission overrides configured across all toolkits.
     *
     * @param concreteJavaClass The resolved concrete class for Java tooling in the active container.
     * @return A consolidated map of tool permission keys (e.g. {@code "SwingJava.compileAndExecute"}) to their permissions.
     */
    public Map<String, ToolPermission> getResolvedToolPermissions(Class<?> concreteJavaClass) {
        if (toolkits == null || toolkits.isEmpty()) {
            return Collections.emptyMap();
        }
        Map<String, ToolPermission> resolved = new HashMap<>();
        for (ToolkitSettings ts : toolkits) {
            resolved.putAll(ts.getResolvedPermissions(concreteJavaClass));
        }
        return Collections.unmodifiableMap(resolved);
    }
}
