/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.java;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;

/**
 * A session-scoped {@link URLClassLoader} that maintains identity and holds
 * in-memory compiled classes ({@link AgiCompiledClass}) across turns for an AGI
 * session.
 * <p>
 * Serving as the parent classloader for ephemeral script loaders (such as
 * {@link Java.AnahataClassLoader}), {@code AgiClassLoader} ensures that classes
 * compiled via {@link Java#compile(String, String, String, String[], String)}
 * are defined exactly once in the session metaspace. This completely eliminates
 * {@link ClassCastException} when instantiating compiled types in one turn and
 * casting them in subsequent turns (e.g. via {@code sessionMap}).
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class AgiClassLoader extends URLClassLoader {

    /**
     * Reference to the owning {@link Java} toolkit for accessing compiled
     * classes, parent-first whitelist, fallback bytes, and sibling loaders.
     */
    private final Java javaToolkit;

    /**
     * Constructs a new {@link AgiClassLoader}.
     *
     * @param urls initial classpath URLs (from extraClassPath entries across
     * compiled classes).
     * @param javaToolkit the owning Java toolkit instance.
     * @param parent the host parent classloader.
     */
    public AgiClassLoader(List<URL> urls, Java javaToolkit, ClassLoader parent) {
        super(urls != null ? urls.toArray(new URL[0]) : new URL[0], parent != null ? parent : javaToolkit.getClass().getClassLoader());
        this.javaToolkit = javaToolkit;
    }

    /**
     * Adds extra classpath directory or JAR entries dynamically to this
     * loader's URLs.
     *
     * @param extraClassPath path-separator delimited classpath string.
     */
    public void addExtraClassPath(String extraClassPath) {
        if (extraClassPath == null || extraClassPath.isBlank()) {
            return;
        }
        for (String entry : extraClassPath.split(File.pathSeparator)) {
            if (!entry.isBlank()) {
                try {
                    addURL(new File(entry.trim()).toURI().toURL());
                } catch (Exception e) {
                    String msg = "AgiClassLoader: Invalid extraClassPath URL '" + entry + "': " + e.getMessage();
                    log.warn(msg, e);
                    javaToolkit.error(msg);
                }
            }
        }
    }

    /**
     * Returns a set of all URLs currently registered on this classloader.
     *
     * @return set of registered URLs.
     */
    public Set<URL> getRegisteredUrls() {
        return new HashSet<>(Arrays.asList(getURLs()));
    }

    /**
     * Checks whether a class has already been loaded and defined by this
     * classloader.
     *
     * @param name the binary name of the class.
     * @return {@code true} if the class is already defined in this loader.
     */
    public boolean isClassLoaded(String name) {
        return findLoadedClass(name) != null;
    }

    /**
     * {@inheritDoc}
     * 
     * Implements session-scoped classloading:
     * <ol>
     * <li>Returns previously loaded classes (guaranteeing single-instance
     * identity).</li>
     * <li>Delegates parent-first infrastructure classes to the host
     * loader.</li>
     * <li>Defines in-memory compiled bytecode from active
     * {@link AgiCompiledClass}es in {@link Java}.</li>
     * <li>Searches child-first URLs for libraries supplied in extra
     * classpath.</li>
     * <li>Falls back to the host parent classloader.</li>
     * <li>Queries toolkit fallback bytes and extra sibling classloaders (e.g.
     * JavaFX).</li>
     * </ol>
     * 
     *
     * @param name the binary name of the class.
     * @param resolve if true, resolves the class.
     * @return the resulting Class object.
     * @throws ClassNotFoundException if the class cannot be found.
     */
    @Override
    protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
        synchronized (getClassLoadingLock(name)) {
            // 1. Check if class is already loaded by this loader (Preserves type identity!)
            Class<?> c = findLoadedClass(name);
            if (c != null) {
                if (resolve) {
                    resolveClass(c);
                }
                return c;
            }

            // 2. PARENT-FIRST for critical framework infrastructure (Agi, ToolContext, etc.)
            if (javaToolkit.getParentFirstClassess().contains(name)) {
                Java.logClassloading("[AgiClassLoader] Delegating infrastructure class to parent: " + name);
                return getParent().loadClass(name);
            }

            // 3. IN-MEMORY COMPILED BYTECODE (from javaToolkit.getAgiCompiledClasses())
            byte[] bytes = findAgiCompiledBytecode(name);
            if (bytes != null) {
                Java.logClassloading("[AgiClassLoader] Defining session in-memory class: " + name);
                c = defineClass(name, bytes, 0, bytes.length);
                if (resolve) {
                    resolveClass(c);
                }
                return c;
            }

            // 4. CHILD-FIRST: URLClassLoader's findClass (for any extraClassPath JARs)
            try {
                c = findClass(name);
                if (resolve) {
                    resolveClass(c);
                }
                return c;
            } catch (ClassNotFoundException ignored) {
            }

            // 5. PARENT-LAST: Delegate to parent (host JVM / NetBeans module / IntelliJ plugin loader)
            try {
                c = super.loadClass(name, resolve);
                return c;
            } catch (ClassNotFoundException parentEx) {
                // 6. SIBLING / FALLBACK: Ask toolkit for MR-JAR fallback bytes or sibling loaders (e.g. JavaFX)
                byte[] fallbackBytes = javaToolkit.findClassFallbackBytes(name);
                if (fallbackBytes != null) {
                    c = defineClass(name, fallbackBytes, 0, fallbackBytes.length);
                    if (resolve) {
                        resolveClass(c);
                    }
                    return c;
                }

                for (ClassLoader extraLoader : javaToolkit.getExtraClassLoaders()) {
                    try {
                        c = extraLoader.loadClass(name);
                        if (c != null) {
                            if (resolve) {
                                resolveClass(c);
                            }
                            return c;
                        }
                    } catch (ClassNotFoundException ignored) {
                    }
                }
                throw parentEx;
            }
        }
    }

    /**
     * Resolves bytecode for a class or any of its inner/nested classes from
     * {@link AgiCompiledClass}es.
     *
     * @param name the binary class name.
     * @return the bytecode byte array, or null if not found.
     */
    private byte[] findAgiCompiledBytecode(String name) {
        var agiClasses = javaToolkit.getAgiCompiledClasses();
        if (agiClasses == null || agiClasses.isEmpty()) {
            return null;
        }

        // 1. Direct match on top-level class
        AgiCompiledClass direct = agiClasses.get(name);
        if (direct != null) {
            byte[] b = direct.getBytecodes().get(name);
            if (b != null) {
                return b;
            }
        }

        // 2. If it's an inner class (e.g. Outer$Inner), check the outer class first
        int dollarIdx = name.indexOf('$');
        if (dollarIdx > 0) {
            String outerName = name.substring(0, dollarIdx);
            AgiCompiledClass outer = agiClasses.get(outerName);
            if (outer != null) {
                byte[] b = outer.getBytecodes().get(name);
                if (b != null) {
                    return b;
                }
            }
        }

        // 3. Fallback scan across all compiled classes
        for (AgiCompiledClass acc : agiClasses.values()) {
            byte[] b = acc.getBytecodes().get(name);
            if (b != null) {
                return b;
            }
        }

        return null;
    }
}
