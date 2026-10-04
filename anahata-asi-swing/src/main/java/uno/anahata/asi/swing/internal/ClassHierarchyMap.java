/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.internal;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * A thread-safe map that resolves registered values for a target class by walking up its superclass hierarchy.
 * <p>
 * Eliminates repetitive while-loop class hierarchy traversals across UI registries
 * (such as toolkit renderers, context provider panels, and parameter renderers).
 * When looking up a mapping for a target class, it checks for an exact match and then
 * progressively inspects each superclass until reaching the configured base bound.
 * </p>
 *
 * @param <B> The base superclass or interface bound for all registered types.
 * @param <V> The value type stored in the registry (e.g., renderer class or factory).
 * @author anahata
 */
@RequiredArgsConstructor
public class ClassHierarchyMap<B, V> {

    /**
     * The upper bound of the class hierarchy traversal.
     */
    @NonNull
    private final Class<B> baseBound;

    /**
     * The backing concurrent registry.
     */
    private final Map<Class<? extends B>, V> registry = new ConcurrentHashMap<>();

    /**
     * Associates the specified value with the given class key.
     *
     * @param key The class key, which must be assignable to the base bound.
     * @param value The value to associate.
     */
    public void put(@NonNull Class<? extends B> key, @NonNull V value) {
        registry.put(key, value);
    }

    /**
     * Resolves the value associated with the target class or its closest registered superclass.
     * <p>
     * Starts from {@code targetClass} and ascends its superclass hierarchy until a registered
     * mapping is found or the traversal exceeds {@code baseBound}.
     * </p>
     *
     * @param targetClass The class to resolve.
     * @return An {@link Optional} containing the resolved value, or empty if none matched.
     */
    public Optional<V> find(Class<?> targetClass) {
        if (targetClass == null) {
            return Optional.empty();
        }
        Class<?> current = targetClass;
        while (current != null && baseBound.isAssignableFrom(current)) {
            V value = registry.get(current);
            if (value != null) {
                return Optional.of(value);
            }
            current = current.getSuperclass();
        }
        return Optional.empty();
    }

    /**
     * Resolves the value associated with the target class, or returns {@code null} if no match is found.
     *
     * @param targetClass The class to resolve.
     * @return The resolved value, or {@code null}.
     */
    public V get(Class<?> targetClass) {
        return find(targetClass).orElse(null);
    }

    /**
     * Removes the mapping for the specified class key.
     *
     * @param key The class key to remove.
     * @return The previous value associated with {@code key}, or {@code null}.
     */
    public V remove(@NonNull Class<? extends B> key) {
        return registry.remove(key);
    }

    /**
     * Removes all mappings from this hierarchy map.
     */
    public void clear() {
        registry.clear();
    }

    /**
     * Checks whether a direct mapping exists for the specified class key.
     *
     * @param key The class key to check.
     * @return true if a direct mapping exists.
     */
    public boolean containsKey(@NonNull Class<? extends B> key) {
        return registry.containsKey(key);
    }

    /**
     * Returns the number of direct class mappings in this hierarchy map.
     *
     * @return The number of direct mappings.
     */
    public int size() {
        return registry.size();
    }
}
