/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;
import lombok.Getter;
import lombok.NonNull;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.tika.utils.ParserUtils;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.status.AgiStatus;
import uno.anahata.asi.persistence.kryo.KryoUtils;
import uno.anahata.asi.agi.event.BasicPropertyChangeSource;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.ResponseModality;

/**
 * A hybrid static/instance class for managing global and application-specific
 * configurations.
 * <ul>
 * <li><b>Static methods</b> provide access to the root Anahata AI working
 * directory and its global subdirectories.</li>
 * <li><b>An instance</b> of this class represents the configuration for a
 * specific host application (e.g., "netbeans", "standalone"), managing its
 * unique preferences and its own application-specific subdirectories.</li>
 * </ul>
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Slf4j
public abstract class AbstractAsiContainer extends BasicPropertyChangeSource {

    // --- STATIC METHODS FOR GLOBAL ACCESS ---
    /**
     * Static initializer to ensure the root working directory exists and is
     * accessible.
     */
    static {
        try {
            Files.createDirectories(getWorkDir());
        } catch (IOException e) {
            throw new RuntimeException("Could not create root work dir: " + getWorkDir(), e);
        }
    }

    /**
     * The unique identifier for the host application (e.g., "netbeans",
     * "standalone").
     * <p>
     * Used to resolve application-specific subdirectories within the root
     * Anahata working directory.</p>
     */
    private final String hostApplicationId;

    /**
     * The persistent preferences for this container instance.
     */
    //private final AsiContainerPreferences preferences;
    /**
     * The list of currently active agi sessions managed by this container.
     */
    private final List<Agi> activeAgis = new ArrayList<>();

    /**
     * A master registry of AI provider instances.
     * <p>
     * Keyed by the provider's unique UUID. These instances are shared across
     * all sessions managed by this container.</p>
     */
    private final Map<String, AbstractAiProvider> providerRegistry = new ConcurrentHashMap<>();

    /**
     * A shared executor for container-level background tasks.
     */
    private final ExecutorService executor;

    /**
     * A JVM-scoped map for tools to store and share objects across all
     * containers, sessions, and turns. This map is thread-safe.
     */
    public static Map applicationAttributes = new ConcurrentHashMap();

    /**
     * A container-scoped map for tools to store objects across all sessions and
     * turns within this specific host application. This map is thread-safe.
     */
    public Map containerAttributes = new ConcurrentHashMap();

    /**
     * Container-level operational notifications and boot diagnostic records.
     */
    private final List<String> notifications = new java.util.concurrent.CopyOnWriteArrayList<>();

    /**
     * Creates a configuration instance for a specific host application. Upon
     * instantiation, it loads the preferences and persisted providers for that
     * application.
     *
     * @param hostApplicationId A unique identifier for the host application
     * (e.g., "netbeans").
     * @throws IOException If creating directories or loading providers fails.
     */
    public AbstractAsiContainer(String hostApplicationId) throws IOException {
        this.hostApplicationId = hostApplicationId;
        //this.preferences = AsiContainerPreferences.load(this);
        //this.preferences.ensureTemplatesInitialized(this);
        this.executor = AsiExecutors.newCachedThreadPoolExecutor(hostApplicationId);

        // Populate the registry from persisted providers on disk
        int diskProviders = loadProvidersFromDisk();
        log.info("Loaded {} AI Providers from disk for host application '{}'", diskProviders, hostApplicationId);
    }

    /**
     * Scans the container's providers directory and loads all serialized AI
     * provider entities.
     *
     * @return The number of providers loaded from disk.
     * @throws IOException If listing the providers directory fails.
     */
    public int loadProvidersFromDisk() throws IOException {
        Path providersDir = getProvidersDir();

        int loadedCount = 0;
        try (Stream<Path> stream = Files.list(providersDir)) {
            List<Path> files = stream.filter(p -> !Files.isDirectory(p))
                    .filter(p -> p.toString().endsWith(".kryo"))
                    .collect(Collectors.toList());

            for (Path file : files) {
                AbstractAiProvider provider;
                try {
                    provider = KryoUtils.loadFromFile(file, AbstractAiProvider.class);
                } catch (Throwable t) {
                    log.error("Failed to deserialize provider from {}. Moving to unloadable directory.", file, t);
                    try {
                        Path unloadablePath = getUnloadableProvidersDir().resolve(file.getFileName());
                        Files.move(file, unloadablePath, StandardCopyOption.REPLACE_EXISTING);
                        log.info("Moved incompatible provider to: {}", unloadablePath);
                        addNotification("Incompatible provider moved to unloadable: " + file.getFileName());
                    } catch (IOException e) {
                        log.error("Failed to move incompatible provider to unloadable directory: {}", file, e);
                    }
                    continue;
                }

                provider.setAsiContainer(this);
                try {
                    provider.initialize();
                } catch (Exception e) {
                    log.error("Provider '{}' failed to initialize and was disabled", provider.getProviderId(), e);
                    provider.setEnabled(false);
                    addNotification("Provider '" + provider.getDisplayName() + "' failed to initialize: " + e.getMessage());
                }
                providerRegistry.put(provider.getUuid(), provider);
                loadedCount++;
            }
        }
        return loadedCount;
    }

    /**
     * Adds an operational notification or startup diagnostic message.
     *
     * @param notification The diagnostic message to record.
     */
    public void addNotification(String notification) {
        if (notification != null && !notification.isBlank()) {
            log.info("[Notification] {}", notification);
            this.notifications.add(notification);
            propertyChangeSupport.firePropertyChange("notifications", null, getNotifications());
        }
    }

    /**
     * Clears all recorded operational notifications.
     */
    public void clearNotifications() {
        this.notifications.clear();
        propertyChangeSupport.firePropertyChange("notifications", null, getNotifications());
    }

    /**
     * Gets an unmodifiable copy of all recorded operational notifications.
     *
     * @return List of notification messages.
     */
    public List<String> getNotifications() {
        return Collections.unmodifiableList(new ArrayList<>(notifications));
    }

    /**
     * Gets all loaded models across all providers from memory.
     *
     * @param providerEnabled if true, filters to only effectively enabled
     * providers (enabled with valid API keys).
     * @param modelEnabled if true, filters to only enabled models
     * (model.isEnabled()).
     * @return an aggregated list of matching models.
     */
    public List<AbstractModel> getAllModels(boolean providerEnabled, boolean modelEnabled) {
        return getAllProviders().stream()
                .filter(p -> !providerEnabled || p.isEffectivelyEnabled())
                .flatMap(p -> (modelEnabled ? p.getEnabledModels() : p.getModels()).stream())
                .collect(Collectors.toList());
    }

    /**
     * Gets all loaded models across all providers from memory.
     *
     * @param providerEnabled if true, filters to only effectively enabled
     * providers.
     * @return an aggregated list of all models.
     */
    public List<AbstractModel> getAllModels(boolean providerEnabled) {
        return getAllModels(providerEnabled, false);
    }

    /**
     * Finds and filters models across providers matching a regex/text query AND
     * all requested response modalities.
     *
     * @param query Optional regex or keyword query.
     * @param modalities Optional list of response modalities (e.g. [IMAGE,
     * AUDIO]).
     * @param enabledOnly If true, filters to only effectively enabled providers
     * (enabled with valid API keys).
     * @return A list of matching {@link AbstractModel} instances across
     * matching providers.
     */
    public List<AbstractModel> findModels(String query, List<ResponseModality> modalities, boolean enabledOnly) {
        return getAllProviders().stream()
                .filter(p -> !enabledOnly || p.isEffectivelyEnabled())
                .flatMap(p -> p.findModels(query, modalities).stream())
                .collect(Collectors.toList());
    }

    /**
     * Retrieves a shared provider instance from the master registry by its
     * UUID.
     * <p>
     * Implementation details: This is the authoritative way to resolve
     * providers. If the UUID is {@code null}, this method returns
     * {@code null}.</p>
     *
     * @param uuid The unique UUID of the provider instance.
     * @return The shared provider instance, or {@code null} if not found.
     */
    public AbstractAiProvider getProvider(String uuid) {
        if (uuid == null) {
            return null;
        }
        return providerRegistry.get(uuid);
    }

    /**
     * Gets an unmodifiable list of all registered provider instances.
     *
     * @return All providers sorted by priority and display name.
     */
    public List<AbstractAiProvider> getAllProviders() {
        return providerRegistry.values().stream()
                .sorted(Comparator.comparingInt(AbstractAiProvider::getPriority)
                        .thenComparing(AbstractAiProvider::getDisplayName, String.CASE_INSENSITIVE_ORDER))
                .collect(Collectors.toList());
    }

    /**
     * Registers a new provider instance in the master registry and persists it
     * to preferences.
     *
     * @param provider The provider instance to register.
     */
    public void registerProvider(@NonNull AbstractAiProvider provider) {
        log.info("Registering AI provider instance: {} ({})", provider.getDisplayName(), provider.getUuid());
        provider.setAsiContainer(this);
        providerRegistry.put(provider.getUuid(), provider);
    }

    /**
     * Unregisters a provider instance, removing it from the registry and the
     * persistent preferences.
     *
     * @param uuid The UUID of the provider to unregister.
     */
    public void unregisterProvider(String uuid) {
        log.info("Unregistering AI provider instance: {}", uuid);
        AbstractAiProvider provider = providerRegistry.remove(uuid);
    }

    /**
     * Finds a registered provider instance of a specific class.
     *
     * @param <T> The provider type.
     * @param providerClass The class to look for.
     * @return The first matching instance, or null if none registered.
     */
    public <T extends AbstractAiProvider> T getProviderByClass(Class<T> providerClass) {
        return providerRegistry.values().stream()
                .filter(providerClass::isInstance)
                .map(providerClass::cast)
                .findFirst()
                .orElse(null);
    }

    /**
     * Resolves the implementation version of the foundational Anahata ASI Core
     * framework.
     * <p>
     * Extracts the version from the package manifest of
     * {@link AbstractAsiContainer} or from the Maven {@code pom.properties}
     * packaged into the Core artifact at build time.
     * </p>
     *
     * @return The Core implementation version string, or {@code null} if
     * running in an unpackaged test environment.
     */
    public static String getAsiCoreImplementationVersion() {
        Package pkg = AbstractAsiContainer.class.getPackage();
        if (pkg != null) {
            String implVer = pkg.getImplementationVersion();
            if (implVer != null && !implVer.isBlank()) {
                return implVer;
            }
        }
        try (var is = AbstractAsiContainer.class.getResourceAsStream("/uno/anahata/asi/version.properties")) {
            if (is != null) {
                java.util.Properties props = new java.util.Properties();
                props.load(is);
                String pomVer = props.getProperty("version");
                if (pomVer != null && !pomVer.isBlank()) {
                    return pomVer;
                }
            }
        } catch (Exception e) {
            log.debug("Could not read Core version.properties: {}", e.getMessage());
        }
        return null;
    }

    /**
     * Resolves the Maven Group ID of this container artifact.
     * <p>
     * Defaults to {@code "uno.anahata"}.
     * <b>Development Notice:</b> This method is used in development mode to
     * locate {@code pom.properties} on the classpath when running directly off
     * {@code target/classes}.
     * </p>
     *
     * @return The Maven groupId string.
     */
    public String getMavenGroupId() {
        return "uno.anahata";
    }

    /**
     * Resolves the Maven Artifact ID of this specific host container module.
     * <p>
     * <b>Development Notice:</b> Subclasses representing specific host
     * environments (e.g. {@code anahata-asi-desktop}, {@code anahata-asi-nb})
     * should override this method so that
     * {@link #getContainerImplementationVersion()} can fish the build version
     * from {@code /META-INF/maven/<groupId>/<artifactId>/pom.properties} when
     * running directly off {@code target/classes} in development mode.
     * </p>
     *
     * @return The Maven artifactId string, or {@code null} if unknown.
     */
    public String getMavenArtifactId() {
        return null;
    }

    /**
     * Resolves the implementation version of this specific host container
     * artifact.
     * <p>
     * <b>Resolution Hierarchy:</b>
     * <ol>
     * <li>Queries {@code getClass().getPackage().getImplementationVersion()}
     * (active in packaged production JARs and NBMs).</li>
     * <li>In development mode (when running off {@code target/classes}), fishes
     * the version from
     * {@code /META-INF/maven/<getMavenGroupId()>/<getMavenArtifactId()>/pom.properties}
     * on the classpath.</li>
     * <li>Falls back to {@link #getAsiCoreImplementationVersion()}.</li>
     * </ol>
     * </p>
     *
     * @return The container implementation version string, or {@code null} if
     * unresolved.
     */
    public String getContainerImplementationVersion() {
        Package pkg = getClass().getPackage();
        if (pkg != null) {
            String implVer = pkg.getImplementationVersion();
            if (implVer != null && !implVer.isBlank()) {
                return implVer;
            }
        }

        String groupId = getMavenGroupId();
        String artifactId = getMavenArtifactId();
        if (groupId != null && artifactId != null) {
            String resPath = "/META-INF/maven/" + groupId + "/" + artifactId + "/pom.properties";
            try (var is = getClass().getResourceAsStream(resPath)) {
                if (is != null) {
                    var props = new java.util.Properties();
                    props.load(is);
                    String pomVer = props.getProperty("version");
                    if (pomVer != null && !pomVer.isBlank()) {
                        return pomVer;
                    }
                }
            } catch (Exception ignored) {
            }
        }

        return getAsiCoreImplementationVersion();
    }

    /**
     * Gets the root working directory for this specific host application
     * instance. e.g., ~/.anahata/asi/netbeans or ~/.anahata/asi/netbeans/1.1.14
     *
     * @return The application-specific working directory path.
     * @throws IOException If creating the directory fails.
     */
    public Path getDirectory() throws IOException {
        Path base = getWorkDirSubDir(hostApplicationId);
        String version = getContainerImplementationVersion();
        if (version != null && !version.isBlank()) {
            return getSubdirectory(base, version);
        }
        return base;
    }

    /**
     * Gets the directory where persisted AI provider entities are stored.
     *
     * @return The providers directory path.
     * @throws IOException If creating the directory fails.
     */
    public Path getProvidersDir() throws IOException {
        return getAppDirSubDir("providers");
    }

    /**
     * Gets the directory where AI provider entities that failed to load are
     * moved.
     *
     * @return The unloadable providers directory path.
     * @throws IOException If creating the directory fails.
     */
    public Path getUnloadableProvidersDir() throws IOException {
        return getSubdirectory(getProvidersDir(), "unloadable");
    }

    /**
     * Gets the directory where AGI session templates are stored.
     *
     * @return The templates directory path.
     * @throws IOException If creating the directory fails.
     */
    public Path getTemplatesDir() throws IOException {
        return getAppDirSubDir("templates");
    }

    /**
     * Gets the directory where AGI templates that failed to load are moved.
     *
     * @return The unloadable templates directory path.
     * @throws IOException If creating the directory fails.
     */
    public Path getUnloadableTemplatesDir() throws IOException {
        return getSubdirectory(getTemplatesDir(), "unloadable");
    }

    /**
     * Gets a named subdirectory within this host application's working
     * directory, creating it if it doesn't exist. e.g.,
     * ~/.anahata/asi/netbeans/sessions
     *
     * @param name The name of the subdirectory.
     * @return The Path to the application-specific subdirectory.
     * @throws IOException If creating the directory fails.
     */
    public Path getAppDirSubDir(String name) throws IOException {
        return getSubdirectory(getDirectory(), name);
    }

    /**
     * Creates a new agi session blueprint. Overridden by concrete containers to
     * provide product-specific configurations (e.g., NetBeansAgiConfig).
     *
     * @return The new agi configuration.
     */
    public abstract AgiConfig createNewAgiConfig();

    /**
     * Gets an unmodifiable list of all registered AI providers that are
     * effectively enabled (the provider is enabled and either does not require
     * an API key or has valid keys configured).
     *
     * @return A list of effectively enabled providers.
     */
    public List<AbstractAiProvider> getEffectivelyEnabledProviders() {
        return getAllProviders().stream()
                .filter(AbstractAiProvider::isEffectivelyEnabled)
                .collect(Collectors.toList());
    }

    /**
     * Checks if any of the AI providers configured in the global template have
     * at least one valid API key.
     *
     * @return true if keys are configured, false otherwise.
     */
    public boolean hasAnyProviderConfigured() {
        return !getEffectivelyEnabledProviders().isEmpty();
    }

    /**
     * Authoritatively creates, configures, registers, and opens a brand-new Agi
     * session using the user's preferred template.
     *
     * @return The newly created and opened Agi session.
     */
    public final Agi createNewAgi() {
        return createNewAgi(createNewAgiConfig());
    }

    /**
     * Authoritatively creates, configures, registers, and opens a brand-new Agi
     * session with the provided configuration.
     * <p>
     * Implementation details: This method orchestrates the creation, initial
     * setup, pooling, and initial opening in one atomic weld. It is the primary
     * entry point for spawning new intelligence instances.</p>
     *
     * @param config The session configuration.
     * @return The newly created and opened Agi session.
     */
    public final Agi createNewAgi(AgiConfig config) {
        Agi agi = new Agi(config);
        configureNewAgi(agi);
        registerInternal(agi);
        open(agi);
        return agi;
    }

    /**
     * authoritatively requests that the specified agi session be opened and
     * brought to the front in the host UI.
     *
     * @param agi The agi session to open.
     */
    public void open(@NonNull Agi agi) {
        boolean stateChanged = !agi.isOpen();
        if (stateChanged) {
            log.info("Requesting open for session: {}", agi.getShortId());
            agi.setOpen(true);
        }

        // Always invoke the hook: if it's already open, the environment 
        // uses this to 'Focus' (select the tab).
        onAgiOpened(agi);
    }

    /**
     * Authoritatively requests that the specified agi session's UI tab or
     * window be closed.
     *
     * @param agi The agi session to close.
     */
    public void close(@NonNull Agi agi) {
        if (!agi.isOpen()) {
            return;
        }

        log.info("Requesting close for session: {}", agi.getShortId());
        agi.setOpen(false);
        onAgiClosed(agi);
    }

    /**
     * Retrieves the platform-specific UI component associated with an Agi
     * session.
     *
     * @param agi The session.
     * @return The UI component (e.g., AgiPanel or AgiTopComponent).
     */
    public abstract Object getUI(Agi agi);

    /**
     * Internal logic for session pooling and common hook invocation.
     *
     * @param agi The session to register.
     */
    private void registerInternal(Agi agi) {
        synchronized (activeAgis) {
            for (Agi existing : activeAgis) {
                if (existing.getConfig().getSessionId().equals(agi.getConfig().getSessionId())) {
                    log.warn("Agi session {} already registered. Skipping.", agi.getConfig().getSessionId());
                    return;
                }
            }
            List<Agi> old = new ArrayList<>(activeAgis);
            activeAgis.add(agi);

            // Common hook for host-aware onboarding
            onAgiRegistered(agi);

            propertyChangeSupport.firePropertyChange("activeAgis", old, Collections.unmodifiableList(activeAgis));
            log.info("Registered agi session: {}", agi.getConfig().getSessionId());
        }
    }

    /**
     * Authoritatively clones an existing Agi session.
     * <p>
     * Implementation details: Performs a deep clone using Kryo, assigns a new
     * unique session ID, and updates the nickname. By placing this logic in the
     * core container, we ensure architectural purity across different UI
     * frameworks.</p>
     *
     * @param agi The source Agi session to clone.
     * @return The newly cloned and registered Agi session.
     */
    public Agi cloneSession(@NonNull Agi agi) {
        log.info("Cloning session: {}", agi.getConfig().getSessionId());
        try {
            Agi clonedAgi = KryoUtils.clone(agi);

            String newSessionId = java.util.UUID.randomUUID().toString();
            clonedAgi.getConfig().setSessionId(newSessionId);

            clonedAgi.bindToContainer(this);
            registerInternal(clonedAgi);

            String currentNick = clonedAgi.getNickname();
            if (currentNick != null && !currentNick.isBlank()) {
                clonedAgi.setNickname(currentNick + " (Clone)");
            } else {
                clonedAgi.setNickname("Clone");
            }

            autoSaveSession(clonedAgi, "cloneSession");
            open(clonedAgi);

            log.info("Session cloned successfully into new session: {}", newSessionId);
            return clonedAgi;
        } catch (Exception e) {
            log.error("Failed to clone session {}", agi.getConfig().getSessionId(), e);
            throw new RuntimeException("Failed to clone session", e);
        }
    }

    /**
     * Registers a newly spawned or cloned Agi session with this container.
     *
     * @param agi The session to register.
     */
    public void registerSession(Agi agi) {
        registerInternal(agi);
    }

    /**
     * Unregisters a agi session from this configuration and triggers host-aware
     * cleanup hooks.
     *
     * @param agi The agi session to unregister.
     */
    public void unregister(Agi agi) {
        synchronized (activeAgis) {
            List<Agi> old = new ArrayList<>(activeAgis);
            if (activeAgis.remove(agi)) {
                onAgiUnregistered(agi);
                propertyChangeSupport.firePropertyChange("activeAgis", old, Collections.unmodifiableList(activeAgis));
                log.info("Unregistered agi session: {}", agi.getConfig().getSessionId());
            }
        }
    }

    /**
     * Gets an unmodifiable list of all active agi sessions.
     *
     * @return The list of active agis.
     */
    public List<Agi> getActiveAgis() {
        synchronized (activeAgis) {
            return Collections.unmodifiableList(new ArrayList<>(activeAgis));
        }
    }

    /**
     * Retrieves an active Agi instance from the container pool by its unique
     * UUID / session ID.
     *
     * @param uuid The unique UUID or session ID of the Agi instance.
     * @return The matching Agi instance, or {@code null} if not found or if
     * uuid is null.
     */
    public Agi getAgi(String uuid) {
        if (uuid == null || uuid.isBlank()) {
            throw new IllegalArgumentException("AGI UUID cannot be null or blank");
        }
        synchronized (activeAgis) {
            Agi agi = activeAgis.stream()
                    .filter(a -> uuid.equals(a.getConfig().getSessionId()))
                    .findFirst()
                    .orElse(null);
            if (agi == null) {
                throw new IllegalArgumentException("No active AGI session found with UUID: " + uuid);
            }
            return agi;
        }
    }

    /**
     * Retrieves all active Agi sessions spawned by a specific parent.
     *
     * @param parentUuid The UUID of the parent session.
     * @return A list of child sessions.
     */
    public List<Agi> getChildrenAgis(String parentUuid) {
        if (parentUuid == null) {
            return Collections.emptyList();
        }
        synchronized (activeAgis) {
            return activeAgis.stream()
                    .filter(a -> parentUuid.equals(a.getConfig().getParentUuid()))
                    .toList();
        }
    }

    /**
     * Gets an unmodifiable list of all agi sessions that are currently
     * logically open in the host UI.
     *
     * @return The list of open agis.
     */
    public List<Agi> getOpenAgis() {
        synchronized (activeAgis) {
            return activeAgis.stream()
                    .filter(Agi::isOpen)
                    .toList();
        }
    }

    /**
     * Hook invoked whenever a session enters the active pool.
     *
     * @param agi The registered session.
     */
    public void onAgiRegistered(Agi agi) {
    }

    /**
     * Hook invoked whenever a session is removed from the active pool.
     *
     * @param agi The unregistered session.
     */
    public void onAgiUnregistered(Agi agi) {
    }

    /**
     * Hook invoked to perform initial post-birth configuration of a new Agi.
     * <p>
     * Implementation details: Applies the global default provider and model
     * from preferences if they are configured.
     * </p>
     *
     * @param agi The new session.
     */
    protected void configureNewAgi(Agi agi) {
        // Apply selected model state to the orchestrator if IDs are present
        if (agi.getConfig().getSelectedModelId() != null) {
            log.info("Applying DNA-defined default model ({}) to new session", agi.getConfig().getSelectedModelId());
            AbstractAiProvider prov = getProvider(agi.getConfig().getSelectedProviderUuid());
            Optional<? extends AbstractModel> am = prov.getModel(agi.getConfig().getSelectedModelId());
            if (am.isPresent()) {
                agi.setSelectedModel(am.get());
            }

        }
    }

    /**
     * Hook invoked when a session has been logically opened.
     *
     * @param agi The opened session.
     */
    protected abstract void onAgiOpened(Agi agi);

    /**
     * Hook invoked when a session has been logically closed.
     *
     * @param agi The closed session.
     */
    protected abstract void onAgiClosed(Agi agi);

    // --- SESSION PERSISTENCE ---
    /**
     * Gets the directory where active agi sessions are stored.
     *
     * @return The sessions directory path.
     * @throws IOException If creating the directory fails.
     */
    @SneakyThrows
    public Path getSessionsDir() {
        return getAppDirSubDir("sessions");
    }

    /**
     * Gets the directory where manually saved agi sessions are stored.
     *
     * @return The saved sessions directory path.
     * @throws IOException If creating the directory fails.
     */
    @SneakyThrows
    public Path getSavedSessionsDir() {
        return getSubdirectory(getSessionsDir(), "saved");
    }

    /**
     * Gets the directory where disposed agi sessions are moved.
     *
     * @return The disposed sessions directory path.
     * @throws IOException If creating the directory fails.
     */
    @SneakyThrows
    public Path getDisposedSessionsDir() {
        return getSubdirectory(getSessionsDir(), "disposed");
    }

    /**
     * Gets the directory where agi sessions that failed to load are moved.
     *
     * @return The unloadable sessions directory path.
     * @throws IOException If creating the directory fails.
     */
    @SneakyThrows
    public Path getUnloadableSessionsDir() {
        return getSubdirectory(getSessionsDir(), "unloadable");
    }

    /**
     * Ensures that a given directory exists on disk, creating it if missing.
     *
     * @param dir The directory path to verify and create.
     * @return The verified directory path.
     * @throws IOException If creating the directory fails.
     */
    public static Path ensureDir(Path dir) throws IOException {
        if (!Files.exists(dir)) {
            Files.createDirectories(dir);
        }
        return dir;
    }

    /**
     * Resolves a named subdirectory against a parent path, ensuring it exists
     * on disk.
     *
     * @param parent The parent directory path.
     * @param subdirName The name of the child directory.
     * @return The verified subdirectory path.
     * @throws IOException If directory creation fails.
     */
    public static Path getSubdirectory(Path parent, String subdirName) throws IOException {
        Path dir = parent.resolve(subdirName);
        return ensureDir(dir);
    }

    /**
     * Performs an automatic backup of the session using Kryo serialization.
     * <p>
     * Implementation details: Only proceeds if the agi is in a stable state
     * (IDLE, TOOL_PROMPT, etc.) to prevent serialization during volatile
     * operations like streaming.</p>
     *
     * @param agi The agi session to save.
     * @param reason A description of why the save was triggered.
     */
    public void autoSaveSession(Agi agi, String reason) throws IOException {
        AgiStatus status = agi.getStatusManager().getCurrentStatus();

        boolean isStable = status == AgiStatus.IDLE
                || status == AgiStatus.TOOL_PROMPT
                || status == AgiStatus.CANDIDATE_CHOICE_PROMPT
                || status == AgiStatus.ERROR
                || status == AgiStatus.AUTO_EXECUTING_TOOLS
                || status == AgiStatus.MAX_RETRIES_REACHED;

        if (!isStable) {
            log.info("Skipping {} auto-save for session {} - agi is currently in volatile state: {}",
                    reason, agi.getConfig().getSessionId(), status);
            return;
        }

        log.info("auto-save for session {} - status: {} - reason:" + reason,
                reason, agi.getConfig().getSessionId(), status);

        saveSessionTo(agi, getSessionsDir());
    }

    /**
     * Manually saves the session to the 'saved' directory.
     *
     * @param agi The agi session to save.
     */
    public void manualSaveSession(Agi agi) throws IOException {
        saveSessionTo(agi, getSavedSessionsDir());
    }

    /**
     * Serializes and saves an Agi session to a specific directory using Kryo.
     * This method is synchronized on the Agi instance to prevent concurrent
     * write issues.
     *
     * @param agi The Agi session to save.
     * @param dir The destination directory.
     * @throws IOException If saving fails.
     */
    private void saveSessionTo(Agi agi, Path dir) throws IOException {
        synchronized (agi) {
            String sessionId = agi.getConfig().getSessionId();
            Path file = dir.resolve(sessionId + ".kryo");
            log.info("Saving session {} to {}", sessionId, file);
            KryoUtils.saveToFile(agi, file);
        }
    }

    /**
     * Permanently disposes of a agi session, shutting it down and moving its
     * serialized file to the 'disposed' directory.
     *
     * @param agi The agi session to dispose.
     */
    @SneakyThrows
    public void dispose(Agi agi) {
        String sessionId = agi.getConfig().getSessionId();
        log.info("Disposing session: {}", sessionId);

        // 0. Authoritatively close the UI if it's open
        close(agi);

        // 1. Shutdown the agi (stops executors, etc.)
        agi.shutdown();

        // 2. Move the session file from active to disposed
        Path activeFile = getSessionsDir().resolve(sessionId + ".kryo");
        if (Files.exists(activeFile)) {
            Path disposedFile = getDisposedSessionsDir().resolve(sessionId + ".kryo");
            Files.move(activeFile, disposedFile, StandardCopyOption.REPLACE_EXISTING);
            log.info("Moved session file to disposed directory: {}", disposedFile);
        }

        // 3. Unregister from active list (fires property change)
        unregister(agi);
    }

    /**
     * Imports an Agi session from an external file. The session is assigned a
     * new ID to avoid collisions and registered as a new active agi.
     *
     * @param path The path to the serialized session file.
     * @return The imported Agi session.
     * @throws IOException If reading the session file fails.
     */
    public Agi importSession(Path path) throws IOException {
        log.info("Importing session from {}", path);
        Agi agi = KryoUtils.loadFromFile(path, Agi.class);

        // Always generate a new session ID for imported sessions to avoid collisions
        agi.getConfig().setSessionId(UUID.randomUUID().toString());

        agi.bindToContainer(this);
        registerInternal(agi);
        return agi;
    }

    /**
     * Scans the sessions directory and loads all serialized Agi sessions. This
     * is typically called during application startup.
     *
     * @return The number of sessions that failed to load.
     * @throws IOException If listing the sessions directory fails.
     */
    public int loadSessions() throws IOException {
        Path sessionsDir = getSessionsDir();

        AtomicInteger failedCount = new AtomicInteger(0);
        try (Stream<Path> stream = Files.list(sessionsDir)) {
            stream.filter(p -> !Files.isDirectory(p)) // Only load files from the root (active sessions)
                    .filter(p -> p.toString().endsWith(".kryo"))
                    .parallel()
                    .forEach(p -> {
                        if (!loadSession(p)) {
                            failedCount.incrementAndGet();
                        }
                    });
        }
        return failedCount.get();
    }

    /**
     * Loads a single Agi session from a file, rebinds it to this container, and
     * registers it.
     *
     * @param path The path to the serialized session file.
     * @return true if the session was loaded successfully, false otherwise.
     */
    private boolean loadSession(Path path) {
        try {
            log.info("Loading session from {}", path);
            Agi agi = KryoUtils.loadFromFile(path, Agi.class);
            agi.bindToContainer(this);
            registerInternal(agi);
            return true;
        } catch (Throwable t) {
            log.error("Failed to load session from {}. Moving to unloadable directory.", path, t);
            try {
                Path unloadablePath = getUnloadableSessionsDir().resolve(path.getFileName());
                Files.move(path, unloadablePath, StandardCopyOption.REPLACE_EXISTING);
                log.info("Moved incompatible session to: {}", unloadablePath);
            } catch (IOException e) {
                log.error("Failed to move incompatible session to unloadable directory: {}", path, e);
            }
            return false;
        }
    }

    /**
     * Shuts down the container and its shared executor.
     */
    public void shutdown() {
        log.info("Shutting down AsiContainer: {}", hostApplicationId);
        executor.shutdown();
    }

    /**
     * Gets the root Anahata AI working directory (e.g., ~/.anahata/asi).
     *
     * @return The root working directory path.
     */
    public static Path getWorkDir() {
        String snapUserData = System.getenv("SNAP_USER_DATA");
        if (snapUserData != null && !snapUserData.isBlank()) {
            return Paths.get(snapUserData, ".anahata", "asi");
        }
        return Paths.get(System.getProperty("user.home"), ".anahata", "asi");
    }

    /**
     * Gets a named subdirectory within the global root working directory,
     * creating it if it doesn't exist. This is used for shared resources like
     * provider configurations. e.g., ~/.anahata/asi/gemini
     *
     * @param name The name of the subdirectory.
     * @return The Path object for the subdirectory.
     */
    @SneakyThrows
    public static Path getWorkDirSubDir(String name) {
        return getSubdirectory(getWorkDir(), name);
    }
}
