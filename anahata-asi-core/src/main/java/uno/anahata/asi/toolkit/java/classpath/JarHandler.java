package uno.anahata.asi.toolkit.java.classpath;

import java.io.File;

/**
 * Defines the contract for extracting metadata from JAR files during 
 * classpath inspection.
 * <p>Implementations of this interface are used by the 
 * {@link uno.anahata.asi.toolkit.java.classpath.VeryPrettyClassPathPrinter} 
 * to identify library versions and vendors, enabling token-efficient 
 * representation of the environment.</p>
 * @author anahata
 */
public interface JarHandler {
    /**
     * Checks if this handler is capable of extracting metadata from the 
     * given JAR file.
     * @param jarFile The file to check.
     * @return true if this handler should be used.
     */
    boolean canHandle(File jarFile);
    /**
     * Extracts semantic metadata (ID, version, vendor) from the JAR file.
     * @param jarFile The file to inspect.
     * @return The extracted metadata, or null if extraction failed.
     */
    JarMetadata extractMetadata(File jarFile);
}
