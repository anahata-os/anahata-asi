/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.internal;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import lombok.extern.slf4j.Slf4j;

/**
 * Dynamic JavaFX runtime bridge for the Swing ASI container.
 * <p>
 * Operates reflectively on the container's designated JavaFX {@link ClassLoader}
 * (which may be a NetBeans module loader, an IDE runtime loader, or the application loader).
 * Avoids direct compile-time linkage errors (such as {@link NoClassDefFoundError}) when running
 * in modular IDE environments where JavaFX is located in a sibling module.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public final class JavaFxBridge {

    private JavaFxBridge() {
    }

    /**
     * Initializes the JavaFX platform on the supplied classloader, sets {@code Platform.setImplicitExit(false)},
     * and queries supported {@code ConditionalFeature}s.
     *
     * @param cl The ClassLoader where JavaFX runtime classes are located.
     * @return formatted JavaFX version and feature string, or {@code null} if unavailable or failed.
     */
    public static String init(ClassLoader cl) {
        if (cl == null) {
            log.info("JavaFxBridge: Supplied ClassLoader is null; JavaFX unavailable.");
            return null;
        }
        log.info("JavaFxBridge: Probing JavaFX runtime on ClassLoader: {}", cl);
        try {
            Class<?> platformClass = cl.loadClass("javafx.application.Platform");
            log.info("JavaFxBridge: Loaded javafx.application.Platform from {}", cl);

            // 1. Ensure startup
            Method startup = platformClass.getMethod("startup", Runnable.class);
            try {
                startup.invoke(null, (Runnable) () -> {});
                log.info("JavaFxBridge: Platform.startup() executed successfully.");
            } catch (InvocationTargetException ite) {
                log.info("JavaFxBridge: Platform was already initialized.");
            } catch (IllegalStateException ignored) {
                log.info("JavaFxBridge: Platform was already initialized.");
            }

            // 2. Lock implicitExit to false so the JavaFX thread never dies
            Method setImplicitExit = platformClass.getMethod("setImplicitExit", boolean.class);
            setImplicitExit.invoke(null, false);
            log.info("JavaFxBridge: Platform.setImplicitExit(false) applied successfully.");

            // 3. Resolve version
            String version = "Available";
            try {
                Class<?> verClass = cl.loadClass("com.sun.javafx.runtime.VersionInfo");
                version = (String) verClass.getMethod("getVersion").invoke(null);
            } catch (Throwable ignored) {
                String sysVer = System.getProperty("javafx.runtime.version");
                if (sysVer != null) {
                    version = sysVer;
                }
            }
            log.info("JavaFxBridge: Detected JavaFX version: {}", version);

            // 4. Query supported conditional features
            List<String> supportedFeatures = new ArrayList<>();
            try {
                Class<?> condFeatureClass = cl.loadClass("javafx.application.ConditionalFeature");
                Object[] enumConstants = condFeatureClass.getEnumConstants();
                Method isSupported = platformClass.getMethod("isSupported", condFeatureClass);
                Method isFxThread = platformClass.getMethod("isFxApplicationThread");
                Method runLater = platformClass.getMethod("runLater", Runnable.class);

                boolean onFx = (Boolean) isFxThread.invoke(null);
                if (onFx) {
                    for (Object c : enumConstants) {
                        if ((Boolean) isSupported.invoke(null, c)) {
                            supportedFeatures.add(c.toString());
                        }
                    }
                } else {
                    CompletableFuture<List<String>> future = new CompletableFuture<>();
                    runLater.invoke(null, (Runnable) () -> {
                        try {
                            List<String> list = new ArrayList<>();
                            for (Object c : enumConstants) {
                                if ((Boolean) isSupported.invoke(null, c)) {
                                    list.add(c.toString());
                                }
                            }
                            future.complete(list);
                        } catch (Throwable t) {
                            future.completeExceptionally(t);
                        }
                    });
                    try {
                        supportedFeatures = future.get(1, TimeUnit.SECONDS);
                    } catch (Throwable t) {
                        log.warn("JavaFxBridge: Timeout waiting for conditional features on FX thread: {}", t.getMessage());
                    }
                }
                log.info("JavaFxBridge: Queried {} supported conditional features: {}", supportedFeatures.size(), supportedFeatures);
            } catch (Throwable t) {
                log.warn("JavaFxBridge: Could not query JavaFX conditional features: {}", t.getMessage());
            }

            StringBuilder sb = new StringBuilder(version);
            if (!supportedFeatures.isEmpty()) {
                sb.append(" [Features: ").append(String.join(", ", supportedFeatures)).append("]");
            }
            String fullInfo = sb.toString();
            log.info("JavaFxBridge: Initialization finished successfully: {}", fullInfo);
            return fullInfo;
        } catch (Throwable t) {
            log.warn("JavaFxBridge: JavaFX not present or initialization failed on ClassLoader {}: {}", cl, t.getMessage());
            return null;
        }
    }
}
