/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.benchmarks;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import lombok.Builder;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.ToolPermission;

/**
 * Encapsulates the toolkit configuration and tool permission mappings for a benchmark test.
 * <p>
 * Binds a target toolkit name or FQN to an optional map of simple tool method names
 * and their respective {@link ToolPermission}s. Automatically resolves composite permission keys
 * (e.g. {@code "SwingJava.compileAndExecute"} or {@code "NbJava.compileAndExecute"}).
 * </p>
 *
 * @param toolkit The simple name (e.g. "Java", "Host") or FQN of the toolkit.
 * @param permissions A map of simple tool method names to their assigned permissions.
 * 
 * @author anahata
 */
@Builder
public record ToolkitSettings(
        String toolkit,
        Map<String, ToolPermission> permissions
) {

    /**
     * Canonical constructor providing unmodifiable copy of permissions.
     *
     * @param toolkit The toolkit name or FQN.
     * @param permissions The tool method permission overrides.
     */
    public ToolkitSettings {
        permissions = permissions != null ? Collections.unmodifiableMap(permissions) : Collections.emptyMap();
    }

    /**
     * Creates a ToolkitSettings entry with no explicit permission overrides by toolkit name.
     *
     * @param toolkit The toolkit simple name or FQN.
     * @return The ToolkitSettings instance.
     */
    public static ToolkitSettings of(String toolkit) {
        return new ToolkitSettings(toolkit, Collections.emptyMap());
    }

    /**
     * Creates a ToolkitSettings entry with a single tool permission override by toolkit name.
     *
     * @param toolkit The toolkit simple name or FQN.
     * @param toolName The simple tool method name (e.g. "compileAndExecute").
     * @param permission The permission override.
     * @return The ToolkitSettings instance.
     */
    public static ToolkitSettings of(String toolkit, String toolName, ToolPermission permission) {
        return new ToolkitSettings(toolkit, Map.of(toolName, permission));
    }

    /**
     * Creates a ToolkitSettings entry with multiple tool permission overrides by toolkit name.
     *
     * @param toolkit The toolkit simple name or FQN.
     * @param permissions Map of simple tool method names to permissions.
     * @return The ToolkitSettings instance.
     */
    public static ToolkitSettings of(String toolkit, Map<String, ToolPermission> permissions) {
        return new ToolkitSettings(toolkit, permissions);
    }

    /**
     * Creates a ToolkitSettings entry with no explicit permission overrides from a Class literal.
     *
     * @param toolkitClass The toolkit class literal.
     * @return The ToolkitSettings instance.
     */
    public static ToolkitSettings of(Class<? extends AnahataToolkit> toolkitClass) {
        return new ToolkitSettings(toolkitClass.getName(), Collections.emptyMap());
    }

    /**
     * Creates a ToolkitSettings entry with a single tool permission override from a Class literal.
     *
     * @param toolkitClass The toolkit class literal.
     * @param toolName The simple tool method name (e.g. "compileAndExecute").
     * @param permission The permission override.
     * @return The ToolkitSettings instance.
     */
    public static ToolkitSettings of(Class<? extends AnahataToolkit> toolkitClass, String toolName, ToolPermission permission) {
        return new ToolkitSettings(toolkitClass.getName(), Map.of(toolName, permission));
    }

    /**
     * Creates a ToolkitSettings entry with multiple tool permission overrides from a Class literal.
     *
     * @param toolkitClass The toolkit class literal.
     * @param permissions Map of simple tool method names to permissions.
     * @return The ToolkitSettings instance.
     */
    public static ToolkitSettings of(Class<? extends AnahataToolkit> toolkitClass, Map<String, ToolPermission> permissions) {
        return new ToolkitSettings(toolkitClass.getName(), permissions);
    }

    /**
     * Resolves the composite tool permission keys for this toolkit using the concrete class simple name.
     *
     * @param concreteClass The resolved concrete class running in the active container.
     * @return An unmodifiable map of resolved tool permission keys.
     */
    public Map<String, ToolPermission> getResolvedPermissions(Class<?> concreteClass) {
        if (permissions.isEmpty()) {
            return Collections.emptyMap();
        }
        String simpleName = (concreteClass != null ? concreteClass.getSimpleName() : getSimpleToolkitName());
        Map<String, ToolPermission> resolved = new HashMap<>();
        permissions.forEach((toolName, perm) -> resolved.put(simpleName + "." + toolName, perm));
        return Collections.unmodifiableMap(resolved);
    }

    /**
     * Extracts the simple unqualified name of the toolkit.
     *
     * @return The simple name.
     */
    public String getSimpleToolkitName() {
        if (toolkit == null) {
            return "Unknown";
        }
        int lastDot = toolkit.lastIndexOf('.');
        return lastDot >= 0 ? toolkit.substring(lastDot + 1) : toolkit;
    }
}
