/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.desktop.tools.benchmarks;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import lombok.Builder;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.ToolPermission;

/**
 * Encapsulates the toolkit configuration and tool permission mappings for a benchmark test.
 * <p>
 * Binds a concrete {@link AnahataToolkit} class literal to an optional map of simple tool method names
 * and their respective {@link ToolPermission}s. Automatically resolves composite permission keys
 * (e.g. {@code "SwingJava.compileAndExecute"}).
 * </p>
 *
 * @param toolkitClass The concrete Class literal of the toolkit.
 * @param permissions A map of simple tool method names to their assigned permissions.
 * 
 * @author anahata
 */
@Builder
public record ToolkitSettings(
        Class<? extends AnahataToolkit> toolkitClass,
        Map<String, ToolPermission> permissions
) {

    /**
     * Canonical constructor providing unmodifiable copy of permissions.
     *
     * @param toolkitClass The toolkit class literal.
     * @param permissions The tool method permission overrides.
     */
    public ToolkitSettings {
        permissions = permissions != null ? Collections.unmodifiableMap(permissions) : Collections.emptyMap();
    }

    /**
     * Creates a ToolkitSettings entry with no explicit permission overrides (inheriting container defaults).
     *
     * @param toolkitClass The toolkit class literal.
     * @return The ToolkitSettings instance.
     */
    public static ToolkitSettings of(Class<? extends AnahataToolkit> toolkitClass) {
        return new ToolkitSettings(toolkitClass, Collections.emptyMap());
    }

    /**
     * Creates a ToolkitSettings entry with a single tool permission override.
     *
     * @param toolkitClass The toolkit class literal.
     * @param toolName The simple tool method name (e.g. "compileAndExecute").
     * @param permission The permission override.
     * @return The ToolkitSettings instance.
     */
    public static ToolkitSettings of(Class<? extends AnahataToolkit> toolkitClass, String toolName, ToolPermission permission) {
        return new ToolkitSettings(toolkitClass, Map.of(toolName, permission));
    }

    /**
     * Creates a ToolkitSettings entry with multiple tool permission overrides.
     *
     * @param toolkitClass The toolkit class literal.
     * @param permissions Map of simple tool method names to permissions.
     * @return The ToolkitSettings instance.
     */
    public static ToolkitSettings of(Class<? extends AnahataToolkit> toolkitClass, Map<String, ToolPermission> permissions) {
        return new ToolkitSettings(toolkitClass, permissions);
    }

    /**
     * Resolves the composite tool permission keys for this toolkit using the class simple name.
     * <p>
     * For example, {@code SwingJava.class} with tool {@code "compileAndExecute"} produces
     * {@code "SwingJava.compileAndExecute" -> ToolPermission}.
     * </p>
     *
     * @return An unmodifiable map of resolved tool permission keys.
     */
    public Map<String, ToolPermission> getResolvedPermissions() {
        if (permissions.isEmpty()) {
            return Collections.emptyMap();
        }
        String simpleName = toolkitClass.getSimpleName();
        Map<String, ToolPermission> resolved = new HashMap<>();
        permissions.forEach((toolName, perm) -> resolved.put(simpleName + "." + toolName, perm));
        return Collections.unmodifiableMap(resolved);
    }
}
