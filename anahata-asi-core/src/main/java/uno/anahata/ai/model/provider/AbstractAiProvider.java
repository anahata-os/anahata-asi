/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.model.provider;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.AsiConfig;

/**
 * The abstract base class for all AI model providers, now with model caching.
 * Its primary responsibilities are to discover available models and manage API keys.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Slf4j
public abstract class AbstractAiProvider {
    private List<String> keyPool;
    private final String providerId;
    private final AtomicInteger round = new AtomicInteger(0);
    
    // Transient cache for the models
    private transient List<? extends AbstractModel> models;

    /**
     * Constructs a new provider instance.
     * @param providerId The unique ID for this provider (e.g., "gemini").
     */
    public AbstractAiProvider(String providerId) {
        this.providerId = providerId;
    }

    /**
     * Fetches the list of all models available from the provider's API.
     * This method is intended to be called by the caching mechanism.
     *
     * @return A list of provider-specific {@link AbstractModel} objects.
     */
    public abstract List<? extends AbstractModel> listModels();

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
     * @return 
     */
    public abstract String getCurrentApiKey();
    
    /**
     * Reloads the keys from the api_keys.txt file
     */
    public void reloadKeyPool() {
        keyPool = readApiKeysFile();
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
        log.debug("Using API key from pool (index {}). Key ends with: {}", nextIdx, key.substring(key.length() - 5));
        return key;
    }

    /**
     * Gets the provider-specific global storage directory within the main AI work directory.
     * 
     * @return The path to the provider's directory.
     */
    public Path getProviderDirectory() {
        return AsiConfig.getWorkDirSubDir(providerId);
    }
    
    public Path getKeysFilePath() {
        Path providerDir = getProviderDirectory();
        Path keysFilePath = providerDir.resolve("api_keys.txt");
        log.info("Keys File Path: " + keysFilePath);

        if (!Files.exists(providerDir)) {
            try {
                log.info("Creating provider directory: {}", providerDir);
                Files.createDirectories(providerDir);
            } catch (IOException e) {
                log.error("Failed to create provider directory at: {}", providerDir, e);
            }
        }

        if (!Files.exists(keysFilePath)) {
            log.warn("API key file not found at {}. Creating a template.", keysFilePath);
            try {
                String template = "# These are your API keys for the '" + getProviderId() + "' provider.\n"
                              + "# Add one key per line.\n"
                              + "# Lines starting with '#' or '//' are treated as comments and ignored.\n"
                              + "# Inline comments using '//' are also supported.\n";
                Files.writeString(keysFilePath, template);
            } catch (IOException e) {
                log.error("Failed to create API key template file at: {}", keysFilePath, e);
            }
            
        }
        return keysFilePath;
    }

    private List<String> readApiKeysFile() {
        
        Path keysFilePath = getKeysFilePath();
        
        try {
            List<String> keys = Files.lines(keysFilePath)
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
