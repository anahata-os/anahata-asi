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

/**
 * The abstract base class for all AI model providers, now with model caching.
 * Its primary responsibilities are to discover available models and manage API keys.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Setter
@Slf4j
public abstract class AbstractAgiProvider {

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
     * The type of tokenizer used by this provider.
     * This determines how accurately the Context Window Garbage Collector
     * can estimate the token count before making an API call.
     */
    private TokenizerType tokenizerType = TokenizerType.ESTIMATE;

    /**
     * Whether this provider is enabled and should be offered to the user.
     */
    private boolean enabled = true;

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
    public AbstractAgiProvider() {
        this.uuid = UUID.randomUUID().toString();
    }

    /**
     * Constructs a new provider instance with a specific UUID.
     * @param uuid The unique ID for this instance.
     */
    public AbstractAgiProvider(String uuid) {
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
     *
     * @return The cached list of models.
     */
    public synchronized List<? extends AbstractModel> getModels() {
        if (this.models == null) {
            log.info("Model cache is empty for provider '{}'. Loading from API...", getProviderId());
            try {
                this.models = listModels();
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
     * Gets the current api key this provider is using.
     * 
     * @return The current API key.
     */
    public abstract String getCurrentApiKey();

    /**
     * Gets the URI where users can acquire API keys for this provider.
     * 
     * @return The acquisition URI.
     */
    public abstract java.net.URI getKeysAcquisitionUri();

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
     * Hook to reset the provider-specific API client (e.g., when keys change).
     * Subclasses should override this to set their native client to null.
     */
    public void hokusPocus() {
        // Default implementation does nothing
    }

    /**
     * Gets the next API key for the specific provider implementation using a round-robin selection from the loaded key pool.
     * 
     * The key pool is reloaded from the file system on every call.
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
        log.info("Hocus Pocus.... Using API key from pool (index {}). Key ends with: {}", nextIdx, key.substring(key.length() - 5));
        return key;
    }

    /**
     * Gets the provider-specific global storage directory within the main AI work directory.
     * 
     * @return The path to the provider's directory.
     */
    public Path getProviderDirectory() {
        String dirName = (folderName != null && !folderName.isBlank()) ? folderName : uuid;
        Path p = Path.of(dirName);
        if (p.isAbsolute()) {
            return p;
        }
        return AbstractAsiContainer.getWorkDirSubDir(dirName);
    }

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

    /**
     * Ensures the API keys file exists on disk, creating it as a 
     * completely empty file if missing.
     */
    public void ensureKeysFileExists() {
        Path path = getKeysFilePath();
        if (!Files.exists(path)) {
            try {
                Files.createFile(path);
                log.info("Created empty API key file at: {}", path);
            } catch (IOException e) {
                log.error("Failed to create empty API key file at: {}", path, e);
            }
        }
    }

    /**
     * Reads the API keys from the provider-specific 'api_keys.txt' file.
     * 
     * @return A list of API keys, or an empty list if the file is missing or empty.
     */
    private List<String> readApiKeysFile() {
        ensureKeysFileExists();
        Path keysFilePath = getKeysFilePath();

        try (Stream<String> lines = Files.lines(keysFilePath)) {
            List<String> keys = lines
                    .map(String::trim)
                    .filter(line -> !line.isEmpty() && !line.startsWith("#") && !line.startsWith("//"))
                    .map(line -> {
                        int commentIndex = line.indexOf("//");
                        return (commentIndex != -1) ? line.substring(0, commentIndex).trim() : line;
                    })
                    .filter(key -> !key.isEmpty())
                    .collect(Collectors.toList());
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
