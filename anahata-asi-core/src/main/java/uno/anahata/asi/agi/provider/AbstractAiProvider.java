/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.agi.provider;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.event.BasicPropertyChangeSource;
import java.util.ArrayList;
import uno.anahata.asi.persistence.kryo.KryoUtils;

/**
 * The abstract base class for all AI model providers, now with model caching.
 * Its primary responsibilities are to discover available models and manage API
 * keys.
 *
 * @author anahata
 */
@Getter
@Setter
@Slf4j
public abstract class AbstractAiProvider extends BasicPropertyChangeSource {

    /**
     * The sorting priority for this provider. Lower values indicate higher
     * priority (pinned to top).
     */
    private int priority = 100;

    /**
     * A transient reference to the parent container. This allows providers to
     * access shared resources like the executor service.
     */
    private transient AbstractAsiContainer asiContainer;

    /**
     * The unique UUID for this specific provider instance. Crucial for
     * distinguishing between multiple instances of the same provider class
     * (e.g., two different Ollama endpoints).
     */
    private String uuid;

    /**
     * The base URL of the provider's API endpoint.
     * <p>
     * This allows the provider to target official cloud services or alternative
     * backends like local proxies, Ollama, or specialized vendor endpoints.
     * </p>
     */
    private String baseUrl;

    /**
     * An optional custom file path for this provider's API keys file. If null
     * or empty, defaults to {@code ~/.anahata/asi/<uuid>_api_keys.txt}.
     */
    private String apiKeysFile;
    /**
     * The user-facing display name for this instance (e.g., 'Groq Cloud').
     */
    private String displayName;

    /**
     * The user-facing description of this instance.
     */
    private String description;

    /**
     * The URI where users can acquire API keys for this provider.
     */
    private String keysAcquisitionUri;

    /**
     * The type of tokenizer used by this provider. This determines how
     * accurately the Context Window Garbage Collector can estimate the token
     * count before making an API call.
     */
    private TokenizerType tokenizerType = TokenizerType.ESTIMATE;

    /**
     * Whether this provider requires an API key to function. If false, the ASI
     * will allow requests even if the key pool is empty (e.g. local Ollama).
     */
    private boolean apiKeyRequired = true;

    /**
     * Whether this provider is enabled and should be offered to the user.
     */
    private boolean enabled = true;

    /**
     * Whether to automatically register and persist newly discovered models
     * when API discovery runs.
     */
    private boolean automaticallyRegisterNewlyDiscoveredModels = true;

    /**
     * The API key currently in use by this provider. Captured during key
     * rotation.
     */
    private transient String currentKey;

    /**
     * The internal cache of loaded API keys, reloaded from disk on change.
     */
    private volatile List<String> keyPool;

    /**
     * Atomic counter for round-robin key selection.
     */
    private final AtomicInteger round = new AtomicInteger(0);

    /**
     * The last-modified timestamp of the API keys file when it was last loaded
     * from disk.
     */
    private transient long keyPoolLastModified = 0;
    /**
     * The master list of persistent model entities for this provider (loaded
     * from disk .kryo files).
     */
    private transient List<AbstractModel> models = new ArrayList<>();

    /**
     * The transient list of models returned directly by the provider's API for
     * diffing and alerting.
     */
    private transient List<AbstractModel> cachedApiModels = new ArrayList<>();

    /**
     * No-arg constructor required for Kryo serialization and dynamic
     * instantiation.
     */
    public AbstractAiProvider() {
        this.uuid = UUID.randomUUID().toString();
    }

    /**
     * Constructs a new provider instance with a specific UUID.
     *
     * @param uuid The unique ID for this instance.
     */
    public AbstractAiProvider(String uuid) {
        this.uuid = uuid;
    }

    /**
     * Initializes this provider after instantiation and container binding.
     * Subclasses can override to perform custom setup before or after calling
     * {@code super.initialize()}.
     *
     * @throws Exception if provider initialization fails.
     */
    public void initialize() throws Exception {
        loadModelsFromDisk();
    }

    /**
     * Persists this AI provider entity directly to disk in the container's
     * providers directory
     * (~/.anahata/asi/&lt;hostApp&gt;/&lt;version&gt;/providers/&lt;uuid&gt;.kryo).
     *
     * @throws java.io.IOException If creating the directory or saving the file
     * fails.
     * @throws java.lang.IllegalStateException If the parent container is null
     * or uuid is null or blank.
     */
    public synchronized void persist() throws IOException {
        if (asiContainer == null) {
            throw new IllegalStateException("Cannot persist provider '" + getDisplayName() + "' because parent ASI container is null");
        }
        if (uuid == null || uuid.isBlank()) {
            throw new IllegalStateException("Cannot persist provider because UUID is null or blank");
        }
        Path targetFile = asiContainer.getProvidersDir().resolve(uuid + ".kryo");
        KryoUtils.saveToFile(this, targetFile);
        log.info("Persisted AI provider '{}' ({}) to {}", getDisplayName(), uuid, targetFile);
    }

    /**
     * Deletes the persisted .kryo file for this AI provider from disk and
     * removes it from its parent container registry.
     *
     * @throws java.io.IOException If deleting the file fails.
     * @throws java.lang.IllegalStateException If the parent container is null
     * or uuid is null or blank.
     */
    public synchronized void remove() throws IOException {
        if (asiContainer == null) {
            throw new IllegalStateException("Cannot remove provider '" + getDisplayName() + "' because parent ASI container is null");
        }
        if (uuid == null || uuid.isBlank()) {
            throw new IllegalStateException("Cannot remove provider because UUID is null or blank");
        }
        Path targetFile = asiContainer.getProvidersDir().resolve(uuid + ".kryo");
        Files.deleteIfExists(targetFile);
        log.info("Deleted persisted provider file for '{}' at {}", getDisplayName(), targetFile);
        asiContainer.unregisterProvider(uuid);
    }
    /**
     * Resolves the path to the models directory for this provider.
     *
     * @return The path to the models storage directory.
     * @throws java.io.IOException If creating the directory fails.
     */
    public Path getModelsDirectory() throws IOException {
        return AbstractAsiContainer.getSubdirectory(getProviderDirectory(), "models");
    }

    /**
     * Gets the directory where model entities that failed to load are quarantined.
     *
     * @return The unloadable models directory path.
     * @throws IOException If creating the directory fails.
     */
    public Path getUnloadableModelsDirectory() throws IOException {
        return AbstractAsiContainer.getSubdirectory(getModelsDirectory(), "unloadable");
    }

    /**
     * Loads all persisted models from this provider's models directory on disk
     * into memory.
     * <p>
     * Implements resilient per-model deserialization: if a model cache file is corrupt or in an
     * incompatible legacy format, it is quarantined to the {@code models/unloadable/} directory and
     * a notification is recorded in the parent container without failing the provider initialization.
     * </p>
     *
     * @return The list of models loaded from disk and bound to this provider.
     * @throws IOException if reading the models directory fails.
     */
    public synchronized List<AbstractModel> loadModelsFromDisk() throws IOException {
        Path modelsDir = getModelsDirectory();
        this.models.clear();
        log.info("{} Deserializing models from {}", getProviderId(), modelsDir);

        List<AbstractModel> loaded = new ArrayList<>();
        try (Stream<Path> stream = Files.list(modelsDir)) {
            List<Path> files = stream.filter(p -> !Files.isDirectory(p))
                    .filter(p -> p.toString().endsWith(".kryo"))
                    .collect(Collectors.toList());
            for (Path file : files) {
                try {
                    byte[] data = Files.readAllBytes(file);
                    AbstractModel model = KryoUtils.deserialize(data, AbstractModel.class);
                    model.setProvider(this);
                    loaded.add(model);
                } catch (Throwable t) {
                    log.warn("Incompatible or corrupted model file '{}' for provider '{}', moving to unloadable: {}", file.getFileName(), getDisplayName(), t.getMessage());
                    try {
                        Path unloadablePath = getUnloadableModelsDirectory().resolve(file.getFileName());
                        Files.move(file, unloadablePath, StandardCopyOption.REPLACE_EXISTING);
                        log.info("Moved incompatible model to: {}", unloadablePath);
                        if (asiContainer != null) {
                            asiContainer.addNotification("Incompatible model cache for '" + getDisplayName() + "' moved to unloadable: " + file.getFileName());
                        }
                    } catch (IOException e) {
                        log.error("Failed to move incompatible model file to unloadable directory: {}", file, e);
                    }
                }
            }
        }
        this.models.addAll(loaded);
        log.info("Loaded {} model(s) from disk for provider '{}'", this.models.size(), getProviderId());
        return this.models;
    }

    /**
     * Adds a new model to this provider's local models list and persists it to
     * disk.
     *
     * @param model The model to add and persist.
     * @throws java.io.IOException if and error occurs persisting it.
     * @throws IllegalArgumentException if model is null or already exists in
     * this provider.
     */
    public synchronized void addModel(AbstractModel model) throws IOException {
        if (model == null) {
            throw new IllegalArgumentException("Model cannot be null");
        }
        if (getModel(model.getModelId()).isPresent()) {
            throw new IllegalArgumentException("Model with ID '" + model.getModelId() + "' already exists in provider '" + getProviderId() + "'");
        }
        model.setProvider(this);
        List<AbstractModel> old = new ArrayList<>(this.models);
        this.models.add(model);
        model.persist();
        propertyChangeSupport.firePropertyChange("models", old, Collections.unmodifiableList(this.models));
    }

    /**
     * Adds multiple models to this provider's local models list, persists each
     * to disk, and fires a single unified property change event for
     * {@code "models"}.
     *
     * @param modelsToAdd The collection of models to add and persist.
     * @throws IOException If saving any model to disk fails.
     * @throws IllegalArgumentException If the list is null or contains a null
     * or duplicate model.
     */
    public synchronized void addModels(List<AbstractModel> modelsToAdd) throws IOException {
        if (modelsToAdd == null || modelsToAdd.isEmpty()) {
            return;
        }
        List<AbstractModel> old = new ArrayList<>(this.models);
        for (AbstractModel model : modelsToAdd) {
            if (model == null) {
                throw new IllegalArgumentException("Model in batch cannot be null");
            }
            if (getModel(model.getModelId()).isPresent()) {
                throw new IllegalArgumentException("Model with ID '" + model.getModelId() + "' already exists in provider '" + getProviderId() + "'");
            }
            model.setProvider(this);
            this.models.add(model);
            model.persist();
        }
        propertyChangeSupport.firePropertyChange("models", old, Collections.unmodifiableList(this.models));
    }

    /**
     * Removes a model from this provider's in-memory list.
     *
     * @param model The model to remove.
     * @throws IllegalArgumentException if model is null.
     */
    public synchronized void removeModel(AbstractModel model) {
        if (model == null) {
            throw new IllegalArgumentException("Model cannot be null");
        }
        List<AbstractModel> old = new ArrayList<>(this.models);
        boolean removed = this.models.removeIf(m -> m.getModelId().equals(model.getModelId()));
        if (removed) {
            propertyChangeSupport.firePropertyChange("models", old, Collections.unmodifiableList(this.models));
        }
    }

    /**
     * Fetches the list of all models available from the provider's remote API.
     *
     * @return A list of provider-specific {@link AbstractModel} objects.
     * @throws java.lang.Exception if network communication, authentication, or
     * parsing fails.
     */
    public abstract List<? extends AbstractModel> listModels() throws Exception;

    ;

    /**
     * Compatibility alias for {@code getUuid()} to maintain integration with
     * existing IDE and UI components that expect a provider ID.
     *
     * @return The unique UUID of this provider instance.
     */
    public String getProviderId() {
        return uuid;
    }

    /**
     * Gets the master list of model entities for this provider (loaded from
     * disk .kryo files).
     *
     * @return The master list of models.
     */
    public synchronized List<AbstractModel> getModels() {
        return this.models;
    }

    /**
     * Gets the list of models that are currently enabled for this provider.
     *
     * @return The list of enabled models.
     */
    public synchronized List<AbstractModel> getEnabledModels() {
        return this.models.stream()
                .filter(AbstractModel::isEnabled)
                .collect(Collectors.toList());
    }

    /**
     * Finds a single model by its unique ID within this provider.
     *
     * @param modelId The ID of the model to find.
     * @return An Optional containing the model if found, otherwise empty.
     */
    public Optional<AbstractModel> getModel(String modelId) {
        return getModels().stream()
                .filter(model -> model.getModelId().equals(modelId))
                .findFirst();
    }

    /**
     * Finds and filters models within this provider matching a regex/text query
     * AND all requested response modalities.
     *
     * @param query Optional regex or text query to match against model ID,
     * display name, description, supported actions, or modalities.
     * @param modalities Optional list of target response modalities (e.g.
     * [IMAGE, AUDIO]). The model must support all listed modalities.
     * @return A list of matching models.
     */
    public List<AbstractModel> findModels(String query, List<ResponseModality> modalities) {
        List<AbstractModel> allModels = getModels();
        if (allModels == null || allModels.isEmpty()) {
            return Collections.emptyList();
        }

        boolean hasQuery = query != null && !query.isBlank();
        boolean hasModalities = modalities != null && !modalities.isEmpty();

        if (!hasQuery && !hasModalities) {
            return new ArrayList<>(allModels);
        }

        Pattern pattern = null;
        if (hasQuery) {
            try {
                pattern = Pattern.compile(query, Pattern.CASE_INSENSITIVE);
            } catch (PatternSyntaxException e) {
                pattern = Pattern.compile(Pattern.quote(query), Pattern.CASE_INSENSITIVE);
            }
        }

        final Pattern finalPattern = pattern;
        final Set<ResponseModality> targetModalities = hasModalities
                ? new HashSet<>(modalities)
                : Collections.emptySet();

        return allModels.stream()
                .filter(m -> m.matches(finalPattern, targetModalities))
                .collect(Collectors.toList());
    }

    /**
     * Queries the provider's remote API to discover models and updates
     * {@link #cachedApiModels}. Binds each model to this provider before
     * updating the cache.
     *
     * @return The list of models discovered directly from the API.
     * @throws java_lang_Exception if remote API communication or model
     * discovery fails.
     */
    public synchronized List<AbstractModel> refreshCachedApiModels() throws Exception {
        log.info("Querying API models for provider '{}'...", getProviderId());
        List<? extends AbstractModel> apiList = listModels();
        if (apiList == null) {
            throw new IllegalStateException("Provider '" + getProviderId() + "' listModels() returned null. Providers must return an empty list or throw an exception.");
        }
        for (AbstractModel m : apiList) {
            m.setProvider(this);
        }
        cachedApiModels = (List<AbstractModel>) apiList;
        return cachedApiModels;
    }

    /**
     * Finds a cached API model by its unique ID.
     *
     * @param modelId The ID of the model to look up in cached API models.
     * @return The cached API model, or null if not found.
     */
    public synchronized AbstractModel getCachedApiModel(String modelId) {
        if (modelId == null || modelId.isBlank()) {
            throw new IllegalArgumentException("modelId cannot be null or blank");
        }
        return cachedApiModels.stream()
                .filter(m -> modelId.equals(m.getModelId()))
                .findFirst()
                .orElse(null);
    }

    /**
     * Gets the combined list of local persisted models and any newly discovered
     * API models that are not yet persisted.
     *
     * @return A list containing all local models followed by unregistered API
     * models.
     */
    public synchronized List<AbstractModel> getAllDisplayModels() {
        List<AbstractModel> result = new ArrayList<>(models);
        Set<String> localIds = models.stream().map(AbstractModel::getModelId).collect(Collectors.toSet());
        for (AbstractModel apiModel : cachedApiModels) {
            if (!localIds.contains(apiModel.getModelId())) {
                result.add(apiModel);
            }
        }
        return result;
    }

    /**
     * Gets a set of all unique supported actions across all models offered by
     * this provider, using the cached model list.
     *
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
    public URI getKeysAcquisitionUri() {
        if (keysAcquisitionUri == null || keysAcquisitionUri.isBlank()) {
            return null;
        }
        try {
            return URI.create(keysAcquisitionUri);
        } catch (Exception e) {
            log.error("Invalid keysAcquisitionUri: {}", keysAcquisitionUri);
            return null;
        }
    }

    /**
     * Gets a template or hint string to display when the API keys file is
     * empty.
     *
     * @return The API key hint text.
     */
    public abstract String getApiKeyHint();

    /**
     * Checks if there are any valid API keys configured for this provider.
     *
     * @return true if at least one key exists.
     */
    public boolean hasKeys() {
        reloadKeyPoolIfNeeded();
        return keyPool != null && !keyPool.isEmpty();
    }

    /**
     * Returns the total number of configured API keys for this provider.
     *
     * @return The count of valid API keys in the key pool.
     */
    public int getKeyPoolSize() {
        reloadKeyPoolIfNeeded();
        return keyPool != null ? keyPool.size() : 0;
    }

    /**
     * Checks if this provider is effectively enabled and ready for active model
     * requests.
     * <p>
     * A provider is effectively enabled if {@link #isEnabled} is {@code true}
     * AND either it does not require an API key (e.g. local Ollama) or at least
     * one valid API key is configured.
     * </p>
     *
     * @return true if enabled and properly keyed for requests.
     */
    public boolean isEffectivelyEnabled() {
        return isEnabled() && (!isApiKeyRequired() || hasKeys());
    }

    /**
     * Markdown table header for provider listings.
     */
    public static final String MARKUP_TABLE_HEADER
            = "| Display Name | UUID | Enabled | Eff. Enabled | Provider Class | Base URL | Key Req. | Key Configured | Keys | Models |\n"
            + "|---|---|---|---|---|---|---|---|---|---|\n";

    /**
     * Formats this provider as a Markdown table row for provider listing tools.
     *
     * @param includeModelIds Whether to include the full comma-separated list
     * of model IDs (true) or just the total count (false).
     * @return A Markdown row representing this provider.
     */
    public String toMarkupRow(boolean includeModelIds) {
        String modelsInfo;
        if (isEnabled() && (!isApiKeyRequired() || hasKeys())) {
            List<? extends AbstractModel> m = getModels();
            if (includeModelIds) {
                modelsInfo = (m != null && !m.isEmpty())
                        ? m.stream().map(AbstractModel::getModelId).collect(Collectors.joining(", "))
                        : "None";
            } else {
                modelsInfo = (m != null) ? String.valueOf(m.size()) : "0";
            }
        } else {
            modelsInfo = "N/A (Disabled)";
        }

        return "| " + (getDisplayName() != null ? getDisplayName() : "N/A")
                + " | " + getUuid()
                + " | " + (isEnabled() ? "✅ YES" : "❌ NO")
                + " | " + (isEffectivelyEnabled() ? "✅ YES" : "❌ NO")
                + " | " + getClass().getName()
                + " | " + (getBaseUrl() != null ? getBaseUrl() : "Default Cloud")
                + " | " + (isApiKeyRequired() ? "YES" : "NO")
                + " | " + (hasKeys() ? "✅ YES" : "❌ NO")
                + " | " + (isApiKeyRequired() ? String.valueOf(getKeyPoolSize()) : "N/A")
                + " | " + modelsInfo
                + " |\n";
    }

    /**
     * Formats this provider as a Markdown table row, defaulting to showing the
     * total count of models.
     *
     * @return A Markdown row representing this provider.
     */
    public String toMarkupRow() {
        return toMarkupRow(false);
    }

    /**
     * Checks if the API keys configuration file on disk has been modified since
     * it was last loaded. If modified, reloads the key pool from disk.
     */
    public synchronized void reloadKeyPoolIfNeeded() {
        Path path = getKeysFilePath();
        if (Files.exists(path)) {
            try {
                long lastModified = Files.getLastModifiedTime(path).toMillis();
                if (keyPool == null || lastModified > keyPoolLastModified) {
                    reloadKeyPool();
                }
            } catch (IOException e) {
                log.error("Failed to check last modified time of API keys file: {}", path, e);
            }
        }
    }

    /**
     * Reloads the API key pool from the provider's configuration file and
     * triggers an initial key selection.
     *
     * @throws java_io_IOException If reading the key file fails.
     */
    public synchronized void reloadKeyPool() throws IOException {
        keyPool = readApiKeysFile();
        if (keyPool.isEmpty()) {
            currentKey = null;
        } else {
            int nextIdx = round.getAndIncrement() % keyPool.size();
            currentKey = keyPool.get(nextIdx);
        }
    }

    /**
     * Returns the API key currently in use by this provider instance, reloading
     * from disk if the file was modified externally.
     *
     * @return The active API key, or {@code null} if the pool is empty.
     */
    public synchronized String getCurrentKey() {
        reloadKeyPoolIfNeeded();
        return currentKey;
    }

    /**
     * Rotates the active key to the next available API key in the pool.
     * Subclasses override to invalidate native API clients.
     */
    public synchronized void hokusPocus() {
        reloadKeyPoolIfNeeded();
        if (keyPool == null || keyPool.isEmpty()) {
            currentKey = null;
            return;
        }
        int nextIdx = round.getAndIncrement() % keyPool.size();
        currentKey = keyPool.get(nextIdx);
        log.info("hokusPocus() completed for {} currentIndex={}", this, nextIdx);
    }

    /**
     * Resolves the absolute path to the provider's storage directory in the
     * active container.
     *
     * @return The path to the provider's configuration and log directory.
     * @throws java.io.IOException If creating the directory fails.
     */
    public Path getProviderDirectory() throws IOException {
        return AbstractAsiContainer.getSubdirectory(getAsiContainer().getProvidersDir(), uuid);
    }

    /**
     * Resolves the path to the API keys configuration file for this provider.
     * Defaults to ~/.anahata/asi/<uuid>_api_keys.txt if apiKeysFile is not
     * specified.
     *
     * @return The path to the API keys configuration file.
     */
    public Path getKeysFilePath() {
        if (apiKeysFile != null && !apiKeysFile.isBlank()) {
            return Path.of(apiKeysFile);
        }
        return AbstractAsiContainer.getWorkDir().resolve(getUuid() + "_api_keys.txt");
    }

    /**
     * Ensures that the API keys file exists on the host filesystem, creating *
     * parent directories and the file if missing.
     */
    public void ensureKeysFileExists() throws IOException {
        Path path = getKeysFilePath();
        if (!Files.exists(path)) {

            Path parent = path.getParent();
            if (parent != null && !Files.exists(parent)) {
                Files.createDirectories(parent);
            }
            Files.createFile(path);
            log.info("Created empty API key file at: {}", path);
        }
    }

    /**
     * Reads and parses the active API keys from the configuration file on disk,
     * recording its last-modified timestamp.
     *
     * @return A list of cleaned, non-empty, non-comment API key strings read
     * from the file.
     * @throws java_io_IOException If reading the file fails.
     */
    private List<String> readApiKeysFile() throws IOException {
        ensureKeysFileExists();
        Path keysFilePath = getKeysFilePath();
        keyPoolLastModified = Files.getLastModifiedTime(keysFilePath).toMillis();
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
                log.info("No active API keys found in {}. Please add your keys to the file if you intend to use this provider.", keysFilePath);
                return Collections.emptyList();
            }
            log.debug("Loaded {} API key(s) for provider '{}' from {}.", keys.size(), getProviderId(), keysFilePath);
            return keys;
        }
    }

}
