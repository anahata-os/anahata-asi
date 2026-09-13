/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.internal;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLClassLoader;
import lombok.extern.slf4j.Slf4j;

/**
 * A specialized bridge {@link ClassLoader} that links Swing components with external JavaFX runtimes.
 * <p>
 * In modular environments such as Apache NetBeans, JavaFX classes (such as {@code javafx.scene.media.MediaPlayer} 
 * or {@code javafx.embed.swing.JFXPanel}) reside in an isolated sibling module (e.g., {@code org.netbeans.libs.javafx}).
 * Because the core Swing module does not statically depend on the JavaFX module, direct class linkage would fail 
 * with a {@link NoClassDefFoundError}.
 * </p>
 * <p>
 * This classloader bridges the two modules at runtime:
 * </p>
 * <ul>
 *   <li>All {@code javafx.*} and {@code com.sun.javafx.*} requests are delegated directly to the container's 
 *       designated JavaFX {@link ClassLoader}.</li>
 *   <li>All other classes (including Swing components and framework models) are resolved through the parent 
 *       Swing module loader.</li>
 * </ul>
 * 
 * @author anahata
 */
@Slf4j
public class JavaFxBridgeClassLoader extends URLClassLoader {

    /**
     * The runtime ClassLoader providing JavaFX classes (e.g. from NetBeans module system).
     */
    private final ClassLoader fxLoader;

    /**
     * Constructs a new JavaFxBridgeClassLoader.
     *
     * @param swingLoader The parent ClassLoader containing Swing and Anahata framework classes.
     * @param fxLoader The ClassLoader containing JavaFX runtime classes.
     */
    public JavaFxBridgeClassLoader(ClassLoader swingLoader, ClassLoader fxLoader) {
        super(new URL[0], swingLoader);
        this.fxLoader = fxLoader;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Intercepts JavaFX package requests and routes them to the JavaFX module loader.
     * </p>
     */
    @Override
    protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
        if (name.startsWith("javafx.") || name.startsWith("com.sun.javafx.")) {
            if (fxLoader != null) {
                try {
                    Class<?> clazz = fxLoader.loadClass(name);
                    if (resolve) {
                        resolveClass(clazz);
                    }
                    return clazz;
                } catch (ClassNotFoundException e) {
                    log.debug("JavaFX ClassLoader could not find class: {}", name);
                }
            }
        }

        if ("uno.anahata.asi.swing.agi.render.JavaFxMediaViewerImpl".equals(name)) {
            Class<?> loaded = findLoadedClass(name);
            if (loaded != null) {
                return loaded;
            }
            String resPath = name.replace('.', '/') + ".class";
            try (InputStream is = getParent().getResourceAsStream(resPath)) {
                if (is != null) {
                    byte[] bytes = is.readAllBytes();
                    Class<?> clazz = defineClass(name, bytes, 0, bytes.length);
                    if (resolve) {
                        resolveClass(clazz);
                    }
                    return clazz;
                }
            } catch (IOException e) {
                log.error("Failed to read bytecode for {}", name, e);
            }
        }

        return super.loadClass(name, resolve);
    }
}
