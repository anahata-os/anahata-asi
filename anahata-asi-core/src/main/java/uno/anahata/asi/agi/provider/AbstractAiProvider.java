/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.agi.provider;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.event.BasicPropertyChangeSource;
import java.util.ArrayList;

/**
 * The abstract base class for all AI model providers, now with model caching.
 * Its primary responsibilities are to discover available models and manage API keys.
 *
 * @author anahata
 */
@Getter
@Setter
@Slf4j
public abstract class AbstractAiProvider extends BasicPropertyChangeSource {

    /**
     * An optional whitelist of model IDs. If not empty, only models matching
     * IDs in this list will be returned by {@link #getModels()}.
     */
    private List<String> allowedModels = new ArrayList<>();

    /**
     * A transient reference to the parent container. 
* This allows providers to access shared resources like the executor service.
     */
    private transient AbstractAsiContainer asiContainer;

    /**
     * The unique UUID for this specific provider instance.
     * Crucial for distinguishing between multiple instances of the same
     * provider class (e.g., two different Ollama endpoints).
     */
    private String uuid;

    /**
     * An optional custom folder name for this provider's configuration.
     * If null or empty, the uuid is used as the directory name.
     */
    private String folderName;

    /**
     * The user-facing display name for this instance (e.g., 'Groq Cloud').
     */
    private String displayName;

    /**
     * The URI where users can acquire API keys for this provider.
     */
    private String keysAcquisitionUri;

    /**
     * The type of tokenizer used by this provider.
     * This determines how accurately the Context Window Garbage Collector
     * can estimate the token count before making an API call.
     */
    private TokenizerType tokenizerType = TokenizerType.ESTIMATE;

    /**
     * Whether this provider requires an API key to function.
     * If false, the ASI will allow requests even if the key pool is empty (e.g. local Ollama).
     */
    private boolean apiKeyRequired = true;

    /**
     * Whether this provider is enabled and should be offered to the user.
     */
    private boolean enabled = true;

    /**
     * The API key currently in use by this provider. Captured during key rotation.
     */
    private transient String currentApiKey;

    /**
     * The internal cache of loaded API keys, reloaded from disk on change. */
    private volatile List<String> keyPool;

    /** Atomic counter for round-robin key selection. */
    private final AtomicInteger round = new AtomicInteger(0);

    /** Lazy-loaded cache of models discovered via the provider's API. */
    private transient List<? extends AbstractModel> models;

    /**
     * No-arg constructor required for Kryo serialization and dynamic instantiation.
     */
    public AbstractAiProvider() {
        this.uuid = UUID.randomUUID().toString();
    }

    /**
     * Constructs a new provider instance with a specific UUID.
     * @param uuid The unique ID for this instance.
     */
    public AbstractAiProvider(String uuid) {
        this.uuid = uuid;
    }

    /**
     * Fetches the list of all models available from the provider's API.
     * This method is intended to be called by the caching mechanism.
     *
     * @return A list of provider-specific {@link AbstractModel} objects.
     */
    public abstract List<? extends AbstractModel> listModels();

    /**
     * Compatibility alias for {@code getUuid()} to maintain integration with existing 
     * IDE and UI components that expect a provider ID.
     * 
     * @return The unique UUID of this provider instance.
     */
    public String getProviderId() {
        return uuid;
    }

    /**
     * Gets the list of models, using a lazy-loaded cache.
     * If the cache is empty, it calls {@link #listModels()} to populate it.
     * If fetching fails, it returns an empty list and caches it to prevent repeated failures.
     * <p>
     * Implementation details: This method automatically filters the results of {@link #listModels()} 
     * against the {@link #allowedModels} whitelist. If the whitelist is empty, all discovered 
     * models are returned.
     * </p>
     *
     * @return The cached list of models.
     */
    public synchronized List<? extends AbstractModel> getModels() {
        if (this.models == null) {
            log.info("Model cache is empty for provider '{}'. Loading from API...", getProviderId());
            try {
                List<? extends AbstractModel> discovered = listModels();
                if (allowedModels != null && !allowedModels.isEmpty()) {
                    this.models = discovered.stream()
                            .filter(m -> allowedModels.contains(m.getModelId()))
                            .collect(Collectors.toList());
                } else {
                    this.models = discovered;
                }
            } catch (Exception e) {
                log.error("Failed to load models for provider '{}'. Caching empty list to prevent repeated errors.", getProviderId(), e);
                this.models = Collections.emptyList();
            }
        }
        return this.models;
    }

    /**
     * Finds a single model by its unique ID within this provider.
     *
     * @param modelId The ID of the model to find.
     * @return An Optional containing the model if found, otherwise empty.
     */
    public Optional<? extends AbstractModel> findModel(String modelId) {
        return getModels().stream()
                .filter(model -> model.getModelId().equals(modelId))
                .findFirst();
    }

    /**
     * Clears the local model cache and forces a reload from the API on the next call to {@link #getModels()}.
     *
     * @return The newly fetched list of models.
     */
    public synchronized List<? extends AbstractModel> refreshModels() {
        log.info("Refreshing model cache for provider '{}'...", getProviderId());
        this.models = null; // Clear the cache
        return getModels();
    }

    /**
     * Gets a set of all unique supported actions across all models offered by this provider, using the cached model list.
     * @return A set of unique action strings.
     */
    public Set<String> getAllSupportedActions() {
        return getModels().stream()
                .flatMap(model -> model.getSupportedActions().stream())
                .collect(Collectors.toCollection(HashSet::new));
    }

    /**
     * Gets the URI where users can acquire API keys for this provider.
     * 
     * @return The acquisition URI, or null if not set.
     */
    public java.net.URI getKeysAcquisitionUri() {
        if (keysAcquisitionUri == null || keysAcquisitionUri.isBlank()) {
            return null;
        }
        try {
            return java.net.URI.create(keysAcquisitionUri);
        } catch (Exception e) {
            log.error("Invalid keysAcquisitionUri: {}", keysAcquisitionUri);
            return null;
        }
    }

    /**
     * Gets a template or hint string to display when the API keys file is empty.
     * 
     * @return The API key hint text.
     */
    public abstract String getApiKeyHint();

    /**
     * Checks if there are any valid (non-comment, non-empty) API keys
     * configured for this provider.
     * 
     * @return true if at least one key exists.
     */
    public boolean hasKeys() {
        return !readApiKeysFile().isEmpty();
    }

    /**
     * Reloads the keys from the api_keys.txt file and triggers a hokusPocus.
     */
    public void reloadKeyPool() {
        keyPool = readApiKeysFile();
        hokusPocus();
    }

    /**
     * Returns the API key currently in use by this provider.
     * If no key is set, it initializes it by calling {@link #getNextKey()}.
     * 
     * @return The current API key, or null if none available.
     */
    public synchronized String getCurrentKey() {
        if (currentApiKey == null) {
            getNextKey();
        }
        return currentApiKey;
    }

    /**
     * Hook to reset the provider-specific API client (e.g., when keys change).
     * Subclasses should override this to set their native client to null.
     * <p>
     * Implementation detail: Triggers a key rotation by calling {@link #getNextKey()}.
     * </p>
     */
    public synchronized void hokusPocus() {
        getNextKey();
    }

    /**
     * Gets the next API key for the specific provider implementation using a 
     * round-robin selection from the loaded key pool.
     * 
     * @return The API key.
     */
    protected String getNextKey() {
        if (keyPool == null) {
            keyPool = readApiKeysFile();
        }
        if (keyPool.isEmpty()) {
            return null;
        }

        // Round-robin key selection
        int nextIdx = round.getAndIncrement() % keyPool.size();
        String key = keyPool.get(nextIdx);
        this.currentApiKey = key;
        log.info("Hocus Pocus.... Using API key from pool (index {}). Key ends with: {}", nextIdx, key.substring(key.length() - 5));
        return key;
    }

    public Path getProviderDirectory() {
        String dirName = (folderName != null && !folderName.isBlank()) ? folderName : uuid;
        Path p = Path.of(dirName);
        if (p.isAbsolute()) {
            return p;
        }
        return AbstractAsiContainer.getWorkDirSubDir(dirName);
    }

    /**
     * Gets the provider-specific global storage directory within the main AI work directory.
     * 
     * @return The path to the provider's directory.
     */
    public Path getKeysFilePath() {
        Path providerDir = getProviderDirectory();
        Path keysFilePath = providerDir.resolve("api_keys.txt");
        log.info("Keys File Path: {}", keysFilePath);
        if (!Files.exists(providerDir)) {
            try {
                log.info("Creating provider directory: {}", providerDir);
                Files.createDirectories(providerDir);
            } catch (IOException e) {
                log.error("Failed to create provider directory at: {}", providerDir, e);
            }
        }
        return keysFilePath;
    }

    public void ensureKeysFileExists() {
        Path path = getKeysFilePath();
        if (!Files.exists(path)) {
            try {
                Files.createFile(path);
                log.info("Created empty API key file at: {}",path);
            } catch (IOException e) {
                log.error("Failed to create empty API key file at: {}", path, e);
            }
        }
    }

    /**
     * Ensures the API keys file exists on disk, creating it as a 
     * completely empty file if missing.
     */
    private List<String> readApiKeysFile() {
        ensureKeysFileExists();
        Path keysFilePath = getKeysFilePath();
        try (Stream<String> lines = Files.lines(keysFilePath)) {
            List<String> keys = lines.map(String::trim).filter(line -> !line.isEmpty() && !line.startsWith("#") && !line.startsWith("//")).map(line -> {
                int commentIndex = line.indexOf("//");
                return (commentIndex != -1) ? line.substring(0, commentIndex).trim() : line;
            }).filter(key -> !key.isEmpty()).collect(Collectors.toList());
            Collections.shuffle(keys);
            if (keys.isEmpty()) {
                log.error("No active API keys found in {}. Please add your keys to the file.", keysFilePath);
                return Collections.emptyList();
            }
            log.debug("Loaded {} API key(s) for provider '{}' from {}.", keys.size(), getProviderId(), keysFilePath);
            return keys;
        } catch (IOException e) {
            log.error("Failed to load API keys from {}. Cannot initialize provider.", keysFilePath, e);
            return Collections.emptyList();
        }
    }

}
