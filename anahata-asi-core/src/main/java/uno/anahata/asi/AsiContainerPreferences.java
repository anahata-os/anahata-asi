/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.internal.kryo.KryoUtils;
import uno.anahata.asi.model.tool.ToolPermission;

/**
 * A unified, serializable POJO that acts as a container for all user-configured preferences.
 * This is the root object that gets persisted to disk and is responsible for its own persistence.
 */
@Getter
@Setter
@Slf4j
public class AsiContainerPreferences {
    private static final String PREFERENCES_FILE_NAME = "preferences.kryo";

    /**
     * Holds all preferences related to tool execution permissions.
     */
    /**
     * A map where the key is the tool name (e.g., "LocalFiles.readFile") and the value
     * is the user's stored preference for that tool.
     */
    private Map<String, ToolPermission> toolPermissions = new HashMap<>();

    // Other preference categories (e.g., ModelPreferences, UIPreferences) can be added here in the future.

    /**
     * Saves the current state of this preferences object to disk.
     *
     * @param config The application-wide configuration, used to determine the correct storage location.
     */
    public synchronized void save(AsiContainer config) {
        Path preferencesFile = getPreferencesFile(config);
        try {
            Files.createDirectories(preferencesFile.getParent());
            log.info("Saving preferences to {}", preferencesFile);
            try (OutputStream os = Files.newOutputStream(preferencesFile)) {
                byte[] bytes = KryoUtils.serialize(this);
                os.write(bytes);
            }
        } catch (IOException e) {
            log.error("Error saving preferences to {}", preferencesFile, e);
        }
    }

    /**
     * Loads the preferences for a given host application from disk.
     *
     * @param config The application-wide configuration.
     * @return The loaded AsiContainerPreferences object, or a new empty one if not found or on error.
     */
    public static synchronized AsiContainerPreferences load(AsiContainer config) {
        Path preferencesFile = getPreferencesFile(config);
        if (Files.exists(preferencesFile)) {
            log.info("Loading preferences from {}", preferencesFile);
            try (InputStream is = Files.newInputStream(preferencesFile)) {
                byte[] bytes = is.readAllBytes();
                return KryoUtils.deserialize(bytes, AsiContainerPreferences.class);
            } catch (Exception e) {
                log.error("Error loading preferences from {}", preferencesFile, e);
            }
        } else {
            log.info("Preferences file not found at {}, creating new preferences.", preferencesFile);
        }
        return new AsiContainerPreferences();
    }

    private static Path getPreferencesFile(AsiContainer config) {
        Path appWorkDir = config.getAppDir();
        return appWorkDir.resolve(PREFERENCES_FILE_NAME);
    }
}
