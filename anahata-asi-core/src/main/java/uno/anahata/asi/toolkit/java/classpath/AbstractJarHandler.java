package uno.anahata.asi.toolkit.java.classpath;

import java.io.File;
import java.io.InputStream;
import java.util.jar.JarFile;
import java.util.jar.Manifest;
import java.util.Scanner;

/**
 * Base class for JAR handlers providing common file system and 
 * manifest reading utilities.
 * @author anahata
 */
public abstract class AbstractJarHandler implements JarHandler {
    /**
     * Reads the Manifest from a JAR file.
     * @param jarFile The JAR file.
     * @return The manifest, or null if missing or unreadable.
     */
    protected Manifest readManifest(File jarFile) {
        try (JarFile jf = new JarFile(jarFile)) {
            return jf.getManifest();
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Reads a specific text entry from within a JAR file.
     * @param jarFile The JAR file.
     * @param path The path to the entry (e.g., "META-INF/maven/pom.properties").
     * @return The entry content, or null if not found.
     */
    protected String readEntry(File jarFile, String path) {
        try (JarFile jf = new JarFile(jarFile)) {
            var entry = jf.getJarEntry(path);
            if (entry == null) return null;
            try (InputStream is = jf.getInputStream(entry);
                 Scanner s = new Scanner(is).useDelimiter("\\A")) {
                return s.hasNext() ? s.next() : "";
            }
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Gets the base name of a JAR file without the extension.
     * @param jarFile The file.
     * @return The base name string.
     */
    protected String getBaseName(File jarFile) {
        String name = jarFile.getName();
        return name.endsWith(".jar") ? name.substring(0, name.length() - 4) : name;
    }
}
