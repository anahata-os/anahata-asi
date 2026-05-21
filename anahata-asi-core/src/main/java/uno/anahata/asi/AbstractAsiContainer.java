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
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.status.AgiStatus;
import uno.anahata.asi.persistence.kryo.KryoUtils;
import uno.anahata.asi.agi.event.BasicPropertyChangeSource;
import uno.anahata.asi.agi.provider.AbstractModel;

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

    /**
     * The unique identifier for the host application (e.g., "netbeans", "standalone"). 
     * <p>Used to resolve application-specific subdirectories within the root 
     * Anahata working directory.</p>
     */
    private final String hostApplicationId;

    /**
     * The persistent preferences for this container instance.
     */
    private final AsiContainerPreferences preferences;

    /**
     * The list of currently active agi sessions managed by this container.
     */
    private final List<Agi> activeAgis = new ArrayList<>();

    /**
     * A master registry of AI provider instances. 
     * <p>Keyed by the provider's unique UUID. These instances are shared 
     * across all sessions managed by this container.</p>
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
     * Creates a configuration instance for a specific host application. Upon
     * instantiation, it loads the preferences for that application.
     *
     * @param hostApplicationId A unique identifier for the host application
     * (e.g., "netbeans").
     */
    public AbstractAsiContainer(String hostApplicationId) {
        this.hostApplicationId = hostApplicationId;
        this.preferences = AsiContainerPreferences.load(this);
        this.preferences.ensureTemplatesInitialized(this);
        this.executor = AsiExecutors.newCachedThreadPoolExecutor(hostApplicationId);

        // Populate the registry from persisted providers
        for (AbstractAiProvider provider : preferences.getRegisteredProviders()) {
            provider.setAsiContainer(this);
            providerRegistry.put(provider.getUuid(), provider);
        }
    }

    /**
     * Retrieves a shared provider instance from the master registry by its UUID. 
     * <p>Implementation details: This is the authoritative way to resolve 
     * providers. If the UUID is {@code null}, this method returns {@code null}.</p>
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
     * @return All providers.
     */
    public List<AbstractAiProvider> getAllProviders() {
        return new ArrayList<>(providerRegistry.values());
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

        List<AbstractAiProvider> registered = preferences.getRegisteredProviders();
        registered.removeIf(p -> p.getUuid().equals(provider.getUuid()));
        registered.add(provider);

        // Sync the template's UUID list
        AgiConfig template = preferences.getAgiTemplate();
        if (template != null && !template.getProviderUuids().contains(provider.getUuid())) {
            template.getProviderUuids().add(provider.getUuid());
        }

        savePreferences();
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
        preferences.getRegisteredProviders().removeIf(p -> p.getUuid().equals(uuid));
        savePreferences();
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
     * Saves the current preferences for this host application to disk.
     */
    public void savePreferences() {
        preferences.save(this);
    }

    /**
     * Gets the root working directory for this specific host application
     * instance. e.g., ~/.anahata/asi/netbeans
     *
     * @return The application-specific working directory path.
     */
    public Path getAppDir() {
        return getWorkDirSubDir(hostApplicationId);
    }

    /**
     * Gets a named subdirectory within this host application's working
     * directory, creating it if it doesn't exist. e.g.,
     * ~/.anahata/asi/netbeans/sessions
     *
     * @param name The name of the subdirectory.
     * @return The Path to the application-specific subdirectory.
     */
    public Path getAppDirSubDir(String name) {
        Path dir = getAppDir().resolve(name);
        try {
            Files.createDirectories(dir);
        } catch (IOException e) {
            log.error("Could not create application subdirectory: {}", dir, e);
        }
        return dir;
    }

    /**
     * Creates a new agi session blueprint. Overridden by concrete containers to
     * provide product-specific configurations (e.g., NetBeansAgiConfig).
     *
     * @return The new agi configuration.
     */
    public abstract AgiConfig createNewAgiConfig();

    /**
     * Checks if any of the AI providers configured in the global template have
     * at least one valid API key.
     *
     * @return true if keys are configured, false otherwise.
     */
    public boolean hasAnyApiKeysConfigured() {
        AgiConfig template = preferences.getAgiTemplate();
        for (String uuid : template.getProviderUuids()) {
            AbstractAiProvider provider = getProvider(uuid);
            if (provider != null && provider.hasKeys()) {
                return true;
            }
        }
        return false;
    }

    /**
     * Authoritatively creates, configures, registers, and opens a brand-new Agi
     * session using the user's preferred template.
     *
     * @return The newly created and opened Agi session.
     */
    public final Agi createNewAgi() {
        return createNewAgi(preferences.createAgiConfig(this));
    }

    /**
     * Authoritatively creates, configures, registers, and opens a brand-new Agi 
     * session with the provided configuration. 
     * <p>Implementation details: This method orchestrates the creation, 
     * initial setup, pooling, and initial opening in one atomic weld. It 
     * is the primary entry point for spawning new intelligence instances.</p>
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
     * Notifies all active sessions that the API keys for a specific provider
     * have been updated.
     *
     * @param uuid The UUID of the provider whose keys changed.
     */
    public void onProviderKeysChanged(String uuid) {
        log.info("Processing API key update for shared provider instance: {}", uuid);
        AbstractAiProvider provider = getProvider(uuid);
        if (provider != null) {
            provider.reloadKeyPool();
        }
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
     * <p>Implementation details: Performs a deep clone using Kryo, assigns a 
     * new unique session ID, and updates the nickname. By placing this logic 
     * in the core container, we ensure architectural purity across different 
     * UI frameworks.</p>
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
        AgiConfig template = preferences.getAgiTemplate();

        if (agi.getConfig().getSelectedProviderUuid() == null) {
            agi.getConfig().setSelectedProviderUuid(template.getSelectedProviderUuid());
        }

        if (agi.getConfig().getSelectedModelId() == null) {
            agi.getConfig().setSelectedModelId(template.getSelectedModelId());
        }

        // Apply selected model state to the orchestrator if IDs are present
        if (agi.getConfig().getSelectedModelId() != null) {
            log.info("Applying DNA-defined default model ({}) to new session", agi.getConfig().getSelectedModelId());
            AbstractAiProvider prov = getProvider(agi.getConfig().getSelectedProviderUuid());
            Optional<? extends AbstractModel> am = prov.findModel(agi.getConfig().getSelectedModelId());
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
     */
    public Path getSessionsDir() {
        return getAppDirSubDir("sessions");
    }

    /**
     * Gets the directory where manually saved agi sessions are stored.
     *
     * @return The saved sessions directory path.
     */
    public Path getSavedSessionsDir() {
        Path dir = getSessionsDir().resolve("saved");
        ensureDirectory(dir);
        return dir;
    }

    /**
     * Gets the directory where disposed agi sessions are moved.
     *
     * @return The disposed sessions directory path.
     */
    public Path getDisposedSessionsDir() {
        Path dir = getSessionsDir().resolve("disposed");
        ensureDirectory(dir);
        return dir;
    }

    /**
     * Gets the directory where agi sessions that failed to load are moved.
     *
     * @return The unloadable sessions directory path.
     */
    public Path getUnloadableSessionsDir() {
        Path dir = getSessionsDir().resolve("unloadable");
        ensureDirectory(dir);
        return dir;
    }

    /**
     * Surgically ensures that a given directory path exists on disk, creating
     * all necessary parent directories if they are missing.
     *
     * @param dir The directory path to verify and create.
     */
    private void ensureDirectory(Path dir) {
        try {
            if (!Files.exists(dir)) {
                Files.createDirectories(dir);
            }
        } catch (IOException e) {
            log.error("Could not create directory: {}", dir, e);
        }
    }

    /**
     * Performs an automatic backup of the session using Kryo serialization. 
     * <p>Implementation details: Only proceeds if the agi is in a stable state 
     * (IDLE, TOOL_PROMPT, etc.) to prevent serialization during volatile 
     * operations like streaming.</p>
     * @param agi    The agi session to save.
     * @param reason A description of why the save was triggered.
     */
    public void autoSaveSession(Agi agi, String reason) {
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
    public void manualSaveSession(Agi agi) {
        saveSessionTo(agi, getSavedSessionsDir());
    }

    /**
     * Serializes and saves a agi session to a specific directory using Kryo.
     * This method is synchronized on the agi instance to prevent concurrent
     * write issues.
     *
     * @param agi The agi session to save.
     * @param dir The destination directory.
     */
    private void saveSessionTo(Agi agi, Path dir) {
        synchronized (agi) {
            String sessionId = agi.getConfig().getSessionId();
            Path file = dir.resolve(sessionId + ".kryo");
            Path tmpFile = dir.resolve(sessionId + ".kryo.tmp");
            try {
                log.info("Saving session {} to {}", sessionId, file);
                byte[] data = KryoUtils.serialize(agi);

                // 1. Write to temporary file
                Files.write(tmpFile, data);

                // 2. Atomic move to destination
                try {
                    Files.move(tmpFile, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
                } catch (java.nio.file.AtomicMoveNotSupportedException e) {
                    log.warn("Atomic move not supported on this filesystem, falling back to standard move for: {}", file);
                    Files.move(tmpFile, file, StandardCopyOption.REPLACE_EXISTING);
                }

            } catch (IOException e) {
                log.error("Failed to save session: {}", sessionId, e);
                // Attempt to clean up the orphaned temp file
                try {
                    Files.deleteIfExists(tmpFile);
                } catch (IOException ex) {
                    log.debug("Could not delete temporary file: {}", tmpFile);
                }
            }
        }
    }

    /**
     * Permanently disposes of a agi session, shutting it down and moving its
     * serialized file to the 'disposed' directory.
     *
     * @param agi The agi session to dispose.
     */
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
            try {
                Path disposedFile = getDisposedSessionsDir().resolve(sessionId + ".kryo");
                Files.move(activeFile, disposedFile, StandardCopyOption.REPLACE_EXISTING);
                log.info("Moved session file to disposed directory: {}", disposedFile);
            } catch (IOException e) {
                log.error("Failed to move session file to disposed directory", e);
            }
        }

        // 3. Unregister from active list (fires property change)
        unregister(agi);
    }

    /**
     * Imports a agi session from an external file. The session is assigned a
     * new ID to avoid collisions and registered as a new active agi.
     *
     * @param path The path to the serialized session file.
     * @return The imported Agi session, or null if import failed.
     */
    public Agi importSession(Path path) {
        try {
            log.info("Importing session from {}", path);
            byte[] data = Files.readAllBytes(path);
            Agi agi = KryoUtils.deserialize(data, Agi.class);

            // Always generate a new session ID for imported sessions to avoid collisions
            agi.getConfig().setSessionId(UUID.randomUUID().toString());

            agi.bindToContainer(this);
            registerInternal(agi);
            return agi;
        } catch (Exception e) {
            log.error("Failed to import session from {}", path, e);
            return null;
        }
    }

    /**
     * Scan the sessions directory and loads all serialized agi sessions. This
     * is typically called during application startup.
     *
     * @return The number of sessions that failed to load.
     */
    public int loadSessions() {
        Path sessionsDir = getSessionsDir();
        if (!Files.exists(sessionsDir)) {
            return 0;
        }

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
        } catch (IOException e) {
            log.error("Failed to list sessions in {}", sessionsDir, e);
        }
        return failedCount.get();
    }

    /**
     * Loads a single agi session from a file, rebinds it to this container, and
     * registers it.
     *
     * @param path The path to the serialized session file.
     * @return true if the session was loaded successfully, false otherwise.
     */
    private boolean loadSession(Path path) {
        try {
            log.info("Loading session from {}", path);
            byte[] data = Files.readAllBytes(path);
            Agi agi = KryoUtils.deserialize(data, Agi.class);
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

    // --- STATIC METHODS FOR GLOBAL ACCESS ---
    /**
     * Static initializer to ensure the root working directory exists.
     */
    static {
        try {
            Files.createDirectories(getWorkDir());
        } catch (IOException e) {
            throw new RuntimeException("Could not create root work dir: " + getWorkDir(), e);
        }
    }

    /**
     * Gets the root Anahata AI working directory (e.g., ~/.anahata/asi).
     *
     * @return The root working directory path.
     */
    public static Path getWorkDir() {
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
    public static Path getWorkDirSubDir(String name) {
        Path dir = getWorkDir().resolve(name);
        try {
            Files.createDirectories(dir);
        } catch (IOException e) {
            log.error("Could not create global subdirectory: {}", dir, e);
        }
        return dir;
    }
}
