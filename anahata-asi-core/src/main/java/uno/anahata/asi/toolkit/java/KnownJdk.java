/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.java;

import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Represents a discovered or configured Java Development Kit (JDK) installation.
 *
 * @param name the display name or identifier (e.g., "corretto-21", "JDK 25", "System (running JVM)")
 * @param homePath the root installation path of the JDK
 * @param javacPath the absolute path to the javac executable (if available)
 * @param version the detected version string (e.g., "21.0.6", "26.0.1"), or null if undetermined
 * @param preferred whether this JDK is considered the primary or project-configured JDK
 *
 * @author anahata
 */
public record KnownJdk(
        String name,
        Path homePath,
        Path javacPath,
        String version,
        boolean preferred) {

    /**
     * Checks whether this JDK has a valid, executable javac compiler binary.
     *
     * @return true if javac exists and is executable.
     */
    public boolean hasCompiler() {
        return javacPath != null && Files.isExecutable(javacPath);
    }
}
