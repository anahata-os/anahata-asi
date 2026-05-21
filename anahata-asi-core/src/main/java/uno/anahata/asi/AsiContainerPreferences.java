/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.event.BasicPropertyChangeSource;
import uno.anahata.asi.persistence.kryo.KryoUtils;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
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
public class AsiContainerPreferences extends BasicPropertyChangeSource {
    private static final String PREFERENCES_FILE_NAME = "preferences.kryo";

    /**
     * The current authoritative version of the AgiConfig DNA.
     * Increment this whenever default policies or configurations are refined in the codebase
     * to trigger a 'Metabolic Drift' detection in the UI.
     */
    public static final int CURRENT_DNA_VERSION = 1;

    /**
     * The version of the DNA template currently stored on disk. 
     * <p>Implementation details: Used to detect if the saved defaults are 
     * from an older version of Anahata and trigger an update if necessary.</p>
     */
    private int dnaVersion = 0;

    /**
     * Flag indicating if the last attempt to load preferences from disk failed.
     * This is transient and used to notify the UI of an 'Evolutionary Wipe' 
     * or structural mismatch during a version upgrade.
     */
    private transient boolean loadFailed = false;

    /**
     * A map where the key is the tool name (e.g., "LocalFiles.readFile") and the value
     * is the user's stored preference for that tool.
     */
    private Map<String, ToolPermission> toolPermissions = new HashMap<>();

    /**
     * The master list of configured AI provider instances (e.g., 'Gemini Native', 'Ollama Local').
     * These instances are serialized directly to disk, allowing custom URLs and tokenizers 
     * to be persisted without code changes.
     */
    private List<AbstractAiProvider> registeredProviders = new ArrayList<>();

    /**
     * A blueprint configuration for new Agi sessions. 
     * This allows users to set global defaults for models, toolkits, and retry policies.
     * <p>
     * <b>DNA Note:</b> The template's selectedProviderClass and selectedModelId fields 
     * serve as the global defaults for the 'Starting XI'.
     * </p>
     */
    private AgiConfig agiTemplate;
    
    /**
     * A blueprint configuration for API requests.
     * This allows users to set global defaults for temperature, max tokens, etc.
     */
    private RequestConfig requestTemplate;

    /**
     * Default constructor ensuring template initialization.
     */
    public AsiContainerPreferences() {
        // Templates are initialized lazily by the container when first accessed
    }

    /**
     * Resets the AgiConfig template to the current runtime defaults. 
     * <p>Implementation details: Invoked when the application version changes 
     * and new toolkits or default policies are introduced to keep the DNA 
     * in sync with the codebase.</p>
     * @param container The ASI container providing the new defaults.
     */
    public void resetAgiTemplate(AbstractAsiContainer container) {
        log.info("Resetting AgiConfig template to factory defaults (Version {})...", CURRENT_DNA_VERSION);
        this.agiTemplate = container.createNewAgiConfig();
        this.agiTemplate.setSessionId("TEMPLATE");
        this.dnaVersion = CURRENT_DNA_VERSION;
        propertyChangeSupport.firePropertyChange("agiTemplate", null, agiTemplate);
    }

    /**
     * Detects if the saved AgiConfig template is out of sync with the 
     * current runtime's factory defaults (e.g. toolkits added or removed).
     * 
     * @param container The ASI container.
     * @return true if a DNA mismatch is detected.
     */
    public boolean isAgiTemplateOutdated(AbstractAsiContainer container) {
        if (agiTemplate == null) {
            return false;
        }
        
        // 1. Version-based detection (for subtle tweaks to defaults)
        if (this.dnaVersion < CURRENT_DNA_VERSION) {
            return true;
        }
        
        // 2. Structural detection (for changes in available toolkits)
        AgiConfig defaults = container.createNewAgiConfig();
        
        Set<Class<?>> templateTools = new HashSet<>(agiTemplate.getToolClasses());
        Set<Class<?>> defaultTools = new HashSet<>(defaults.getToolClasses());
        
        return !templateTools.equals(defaultTools);
    }

    /**
     * Ensures that the templates are initialized using the provided container 
     * as a reference for defaults.
     * 
     * @param container The ASI container.
     */
    public void ensureTemplatesInitialized(AbstractAsiContainer container) {
        if (agiTemplate == null) {
            log.info("Initializing global AgiConfig template...");
            agiTemplate = container.createNewAgiConfig();
            // Templates don't need unique session IDs
            agiTemplate.setSessionId("TEMPLATE");
        }
        if (requestTemplate == null) {
            log.info("Initializing global RequestConfig template...");
            requestTemplate = new RequestConfig(null); // No parent agi for template
        }
    }

    /**
     * Creates a deep-cloned instance of the AgiConfig template, bound to the 
     * specified container and assigned a fresh session ID.
     * 
     * @param container The container to bind the new config to.
     * @return A fresh AgiConfig clone, or a new default config if no template exists.
     */
    public AgiConfig createAgiConfig(@NonNull AbstractAsiContainer container) {
        ensureTemplatesInitialized(container);
        AgiConfig clone = KryoUtils.clone(agiTemplate);
        clone.setAsiContainer(container);
        // Ensure the new session gets its own unique identity
        clone.setSessionId(UUID.randomUUID().toString());
        
        // Sync: ensure all currently registered enabled providers are available in the new session
        for (AbstractAiProvider p : container.getAllProviders()) {
            if (p.isEnabled() && !clone.getProviderUuids().contains(p.getUuid())) {
                clone.getProviderUuids().add(p.getUuid());
            }
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
        ensureTemplatesInitialized(agi.getConfig().getAsiContainer());
        RequestConfig clone = KryoUtils.clone(requestTemplate);
        clone.setAgi(agi);
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
            } catch (AtomicMoveNotSupportedException e) {
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
     * <p>Implementation details: Uses {@link uno.anahata.asi.persistence.kryo.KryoUtils} 
     * to deserialize the preferences. If loading fails due to corruption or 
     * evolutionary mismatch, the file is backed up and a fresh preferences 
     * object is returned.</p>
     * @param config The application-wide container.
     * @return The loaded preferences or a new instance on failure.
     */
    public static synchronized AsiContainerPreferences load(AbstractAsiContainer config) {
        Path preferencesFile = getPreferencesFile(config);
        if (Files.exists(preferencesFile)) {
            log.info("Loading preferences from {}", preferencesFile);
            try (InputStream is = Files.newInputStream(preferencesFile)) {
                byte[] bytes = is.readAllBytes();
                return KryoUtils.deserialize(bytes, AsiContainerPreferences.class);
            } catch (Throwable t) {
                log.error("Error loading preferences from {}. An evolutionary mismatch or corruption was detected.", preferencesFile, t);
                try {
                    Path backup = preferencesFile.resolveSibling(PREFERENCES_FILE_NAME + ".broken." + System.currentTimeMillis());
                    Files.move(preferencesFile, backup, StandardCopyOption.REPLACE_EXISTING);
                    log.info("Moved incompatible preferences to: {}", backup);
                } catch (IOException ex) {
                    log.error("Failed to backup broken preferences", ex);
                }
                AsiContainerPreferences prefs = new AsiContainerPreferences();
                prefs.setLoadFailed(true);
                return prefs;
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
