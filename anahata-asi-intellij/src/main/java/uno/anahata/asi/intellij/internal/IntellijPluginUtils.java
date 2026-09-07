/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.internal;

import com.intellij.ide.plugins.IdeaPluginDependency;
import com.intellij.ide.plugins.IdeaPluginDescriptor;
import com.intellij.ide.plugins.PluginManagerCore;
import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.extensions.PluginId;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;

/**
 * Utility class for introspecting the IntelliJ IDEA runtime and assembling the full plugin classpath.
 * <p>
 * Mirrors the dependency resolution architecture of {@code NetBeansModuleUtils} by dynamically
 * discovering the plugin descriptor, recursively traversing declared plugin dependencies (such as
 * the IntelliJ Platform, Java plugin, Maven integration, and Terminal support), and aggregating
 * all required library JARs without hardcoding specific paths or IDs.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public final class IntellijPluginUtils {

    /**
     * The official plugin ID of the Anahata ASI IntelliJ integration.
     */
    public static final String PLUGIN_ID = "uno.anahata.asi.intellij";

    /**
     * Cached classpath string for the IntelliJ plugin runtime environment.
     */
    private static String cachedAnahataAsiPluginClasspath;

    /**
     * Private constructor to prevent instantiation of this utility class.
     */
    private IntellijPluginUtils() {
    }

    /**
     * Returns the comprehensive classpath string for the Anahata ASI IntelliJ plugin runtime.
     * The result is cached after the first computation.
     *
     * @return the fully assembled classpath string separated by {@link File#pathSeparator}.
     */
    public static synchronized String getFullAnahataAsiPluginClasspath() {
        if (cachedAnahataAsiPluginClasspath == null) {
            cachedAnahataAsiPluginClasspath = buildFullPluginClasspath();
        }
        return cachedAnahataAsiPluginClasspath;
    }

    /**
     * Resets the cached plugin classpath, forcing re-resolution on subsequent accesses.
     */
    public static synchronized void resetCachedPluginClasspath() {
        cachedAnahataAsiPluginClasspath = null;
        log.info("IntellijPluginUtils: Cached plugin classpath has been reset.");
    }

    /**
     * Assembles the complete runtime classpath by traversing the plugin descriptor and its declared
     * dependencies, the IntelliJ Platform core libraries, and the system classpath.
     *
     * @return the combined classpath string.
     */
    public static String buildFullPluginClasspath() {
        Set<String> paths = new LinkedHashSet<>();
        Set<PluginId> processedPlugins = new HashSet<>();

        // 1. Discover the root Anahata ASI IntelliJ plugin descriptor
        PluginId rootPluginId = PluginId.getId(PLUGIN_ID);
        IdeaPluginDescriptor rootDescriptor = PluginManagerCore.getPlugin(rootPluginId);
        if (rootDescriptor != null) {
            collectPluginJars(rootDescriptor, processedPlugins, paths);
        } else {
            log.warn("IntellijPluginUtils: Root plugin descriptor not found for ID '{}'. Falling back to platform libraries.", PLUGIN_ID);
        }

        // 2. Collect core IntelliJ Platform library JARs from PathManager.getLibPath()
        try {
            String libPath = PathManager.getLibPath();
            if (libPath != null) {
                Path platformLib = Path.of(libPath);
                if (Files.exists(platformLib)) {
                    if (Files.isDirectory(platformLib)) {
                        try (Stream<Path> stream = Files.walk(platformLib)) {
                            stream.filter(Files::isRegularFile)
                                    .filter(p -> p.getFileName().toString().endsWith(".jar"))
                                    .forEach(p -> paths.add(p.toAbsolutePath().toString()));
                        }
                    } else if (Files.isRegularFile(platformLib) && platformLib.getFileName().toString().endsWith(".jar")) {
                        paths.add(platformLib.toAbsolutePath().toString());
                    }
                }
            }
        } catch (IOException e) {
            log.error("IntellijPluginUtils: Failed to traverse IntelliJ Platform libraries in PathManager.getLibPath()", e);
        }

        // 3. Aggregate valid existing system classpath entries
        String sysCp = System.getProperty("java.class.path");
        if (sysCp != null && !sysCp.isBlank()) {
            for (String entry : sysCp.split(File.pathSeparator)) {
                if (!entry.isBlank() && Files.exists(Path.of(entry))) {
                    paths.add(entry);
                }
            }
        }

        log.info("IntellijPluginUtils: Assembled full plugin classpath with {} distinct entries.", paths.size());
        return String.join(File.pathSeparator, paths);
    }

    /**
     * Recursively traverses a plugin descriptor and its declared dependencies, collecting all
     * packaged JAR files from each plugin's {@code lib} directory.
     *
     * @param descriptor       the plugin descriptor to inspect.
     * @param processedPlugins set of already visited plugin IDs to prevent circular dependency loops.
     * @param paths            the accumulator set for resolved JAR file paths.
     */
    private static void collectPluginJars(IdeaPluginDescriptor descriptor, Set<PluginId> processedPlugins, Set<String> paths) {
        PluginId id = descriptor.getPluginId();
        if (id != null && !processedPlugins.add(id)) {
            return;
        }

        // Collect JARs belonging to this plugin (packaged in its lib/ directory or as a single JAR)
        Path pluginPath = descriptor.getPluginPath();
        if (pluginPath != null && Files.exists(pluginPath)) {
            if (Files.isRegularFile(pluginPath) && pluginPath.getFileName().toString().endsWith(".jar")) {
                paths.add(pluginPath.toAbsolutePath().toString());
            } else if (Files.isDirectory(pluginPath)) {
                Path libDir = pluginPath.resolve("lib");
                if (Files.exists(libDir) && Files.isDirectory(libDir)) {
                    try (Stream<Path> stream = Files.walk(libDir)) {
                        stream.filter(Files::isRegularFile)
                                .filter(p -> p.getFileName().toString().endsWith(".jar"))
                                .forEach(p -> paths.add(p.toAbsolutePath().toString()));
                    } catch (IOException e) {
                        log.error("IntellijPluginUtils: Failed to read JAR files from plugin lib directory: {}", libDir, e);
                    }
                }
            }
        }

        // Recursively traverse all declared plugin dependencies without hardcoding
        List<? extends IdeaPluginDependency> dependencies = descriptor.getDependencies();
        if (dependencies != null) {
            for (IdeaPluginDependency dep : dependencies) {
                PluginId depId = dep.getPluginId();
                if (depId != null && !processedPlugins.contains(depId)) {
                    IdeaPluginDescriptor depDescriptor = PluginManagerCore.getPlugin(depId);
                    if (depDescriptor == null) {
                        depDescriptor = PluginManagerCore.findPluginByPlatformAlias(depId);
                    }
                    if (depDescriptor != null) {
                        collectPluginJars(depDescriptor, processedPlugins, paths);
                    } else {
                        log.debug("IntellijPluginUtils: Optional or unresolvable dependency descriptor for plugin ID: {}", depId);
                    }
                }
            }
        }
    }
}
