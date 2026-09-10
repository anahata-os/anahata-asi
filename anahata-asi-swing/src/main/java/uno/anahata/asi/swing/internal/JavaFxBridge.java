/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import javafx.application.ConditionalFeature;
import javafx.application.Platform;
import lombok.extern.slf4j.Slf4j;

/**
 * Type-safe JavaFX runtime bridge for the Swing ASI container.
 * <p>
 * Isolates direct references to {@link Platform} and {@link ConditionalFeature}
 * so that environments without JavaFX on the classpath do not trigger
 * {@link NoClassDefFoundError} during container initialization.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public final class JavaFxBridge {

    private JavaFxBridge() {
    }

    /**
     * Initializes the JavaFX platform, sets {@code Platform.setImplicitExit(false)},
     * and queries supported {@link ConditionalFeature}s.
     *
     * @return formatted JavaFX version and feature string.
     */
    public static String init() {
        try {
            try {
                Platform.startup(() -> {});
            } catch (IllegalStateException ignored) {
                // Already initialized
            }
            Platform.setImplicitExit(false);

            String version = "Available";
            try {
                Class<?> verClass = Class.forName("com.sun.javafx.runtime.VersionInfo");
                version = (String) verClass.getMethod("getVersion").invoke(null);
            } catch (Throwable ignored) {
                String sysVer = System.getProperty("javafx.runtime.version");
                if (sysVer != null) {
                    version = sysVer;
                }
            }

            List<String> supportedFeatures = new ArrayList<>();
            try {
                if (Platform.isFxApplicationThread()) {
                    for (ConditionalFeature f : ConditionalFeature.values()) {
                        if (Platform.isSupported(f)) {
                            supportedFeatures.add(f.name());
                        }
                    }
                } else {
                    CompletableFuture<List<String>> future = new CompletableFuture<>();
                    Platform.runLater(() -> {
                        try {
                            List<String> list = new ArrayList<>();
                            for (ConditionalFeature f : ConditionalFeature.values()) {
                                if (Platform.isSupported(f)) {
                                    list.add(f.name());
                                }
                            }
                            future.complete(list);
                        } catch (Throwable t) {
                            future.completeExceptionally(t);
                        }
                    });
                    supportedFeatures = future.get(3, TimeUnit.SECONDS);
                }
            } catch (Throwable t) {
                log.debug("Could not query JavaFX conditional features: {}", t.getMessage());
            }

            StringBuilder sb = new StringBuilder(version);
            if (!supportedFeatures.isEmpty()) {
                sb.append(" [Features: ").append(String.join(", ", supportedFeatures)).append("]");
            }
            String fullInfo = sb.toString();
            log.info("JavaFX Platform initialized with Platform.setImplicitExit(false). Version: {}", fullInfo);
            return fullInfo;
        } catch (Throwable t) {
            log.error("Failed to initialize JavaFX Platform in JavaFxBridge: {}", t.getMessage(), t);
            return null;
        }
    }
}
