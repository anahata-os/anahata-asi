/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.HashMap;
import java.util.Map;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.persistence.kryo.KryoUtils;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.provider.RequestConfig;
import uno.anahata.asi.agi.tool.ToolPermission;

/**
 * A unified, serializable POJO that acts as a container for all user-configured preferences.
 * This is the root object that gets persisted to disk and is responsible for its own persistence.
 * 
 * Logic: Uses the atomic save pattern (write to .tmp then ATOMIC_MOVE) to prevent corruption.
 */
@Getter
@Setter
@Slf4j
public class AsiContainerPreferences {
    private static final String PREFERENCES_FILE_NAME = "preferences.kryo";

    /**
     * A map where the key is the tool name (e.g., "LocalFiles.readFile") and the value
     * is the user's stored preference for that tool.
     */
    private Map<String, ToolPermission> toolPermissions = new HashMap<>();

    /**
     * A blueprint configuration for new Agi sessions. 
     * This allows users to set global defaults for models, toolkits, and retry policies.
     */
    private AgiConfig agiTemplate;
    
    /**
     * A blueprint configuration for API requests.
     * This allows users to set global defaults for temperature, max tokens, etc.
     */
    private RequestConfig requestTemplate;

    /**
     * Creates a deep-cloned instance of the AgiConfig template, bound to the 
     * specified container and assigned a fresh session ID.
     * 
     * @param container The container to bind the new config to.
     * @return A fresh AgiConfig clone, or a new default config if no template exists.
     */
    public AgiConfig createAgiConfig(@NonNull AbstractAsiContainer container) {
        AgiConfig clone;
        if (agiTemplate != null) {
            clone = KryoUtils.clone(agiTemplate);
            clone.setAsiContainer(container);
            // Ensure the new session gets its own unique identity
            clone.setSessionId(java.util.UUID.randomUUID().toString());
        } else {
            clone = container.createNewAgiConfig();
        }
        return clone;
    }

    /**
     * Creates a deep-cloned instance of the RequestConfig template, bound to the 
     * specified Agi session.
     * 
     * @param agi The Agi session to bind the new request config to.
     * @return A fresh RequestConfig clone, or a new default config if no template exists.
     */
    public RequestConfig createRequestConfig(@NonNull Agi agi) {
        RequestConfig clone;
        if (requestTemplate != null) {
            clone = KryoUtils.clone(requestTemplate);
            clone.setAgi(agi);
        } else {
            clone = new RequestConfig(agi);
        }
        return clone;
    }

    /**
     * Saves the current state of this preferences object to disk.
     * Uses an atomic write pattern to ensure data integrity.
     *
     * @param config The application-wide configuration, used to determine the correct storage location.
     */
    public synchronized void save(AbstractAsiContainer config) {
        Path preferencesFile = getPreferencesFile(config);
        Path tmpFile = preferencesFile.resolveSibling(PREFERENCES_FILE_NAME + ".tmp");
        
        try {
            Files.createDirectories(preferencesFile.getParent());
            log.info("Saving preferences to {}", preferencesFile);
            
            byte[] bytes = KryoUtils.serialize(this);
            
            // 1. Write to temporary file
            Files.write(tmpFile, bytes);
            
            // 2. Atomic move to destination
            try {
                Files.move(tmpFile, preferencesFile, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
            } catch (java.nio.file.AtomicMoveNotSupportedException e) {
                log.warn("Atomic move not supported on this filesystem, falling back to standard move for preferences.");
                Files.move(tmpFile, preferencesFile, StandardCopyOption.REPLACE_EXISTING);
            }
            
        } catch (IOException e) {
            log.error("Error saving preferences to {}", preferencesFile, e);
            try {
                Files.deleteIfExists(tmpFile);
            } catch (IOException ex) {
                // Ignore cleanup error
            }
        }
    }

    /**
     * Loads the preferences for a given host application from disk.
     *
     * @param config The application-wide configuration.
     * @return The loaded AsiContainerPreferences object, or a new empty one if not found or on error.
     */
    public static synchronized AsiContainerPreferences load(AbstractAsiContainer config) {
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

    private static Path getPreferencesFile(AbstractAsiContainer config) {
        Path appWorkDir = config.getAppDir();
        return appWorkDir.resolve(PREFERENCES_FILE_NAME);
    }
}
