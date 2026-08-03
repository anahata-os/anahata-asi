/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.javafx.util;

import javafx.application.Platform;
import lombok.extern.slf4j.Slf4j;

/**
 * Utility for initializing and managing the JavaFX runtime lifecycle across ASI sessions.
 * 
 * @author anahata
 */
@Slf4j
public class JavaFxUtils {

    private static boolean initialized = false;

    /**
     * Ensures the JavaFX Platform is booted exactly once per JVM with implicitExit set to false.
     */
    public static synchronized void ensureInitialized() {
        if (initialized) {
            return;
        }
        try {
            Platform.startup(() -> {});
            Platform.setImplicitExit(false);
            initialized = true;
            log.info("JavaFX Platform successfully initialized with setImplicitExit(false).");
        } catch (IllegalStateException e) {
            try {
                Platform.setImplicitExit(false);
            } catch (Throwable ignored) {}
            initialized = true;
        } catch (Throwable t) {
            log.error("Failed to initialize JavaFX Platform: {}", t.getMessage(), t);
        }
    }
}
